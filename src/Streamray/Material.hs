{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module represents material, i.e. surface behaviors
module Streamray.Material where

import Control.DeepSeq
import GHC.Generics
import Streamray.Linear

-- | How the surface behaves with light.
data MaterialBehavior
  = -- | A diffuse material, which scatters light in every directions
    Diffuse
  | -- | A glass material, not used yet
    Glass Float
  | -- | A mirror material, not used yet
    Mirror
  deriving (Show, NFData, Generic)

-- | Material, with albedo and behavior
data Material = Material
  { albedo :: V3 'Color,
    behavior :: MaterialBehavior,
    emission :: V3 'Color
  }
  deriving (Show, NFData, Generic)

-- | Compute the reflexion vector
--       /
--      /
--     /
--  wi/       wo>
--    \   n   /
--     \  |  /
--      \ | /
--       >|/
--     ---x-----
-- >>> We can see from this graph that wo - wi (using the parallelogram to
-- report vectors) equals a vector proportional to the normal.
--
-- wo - wi = f * normal
-- ==>
-- wo = f * normal - wi
--
-- >>> We also observe that the height of the parallelogram is equal to two times the height of wi
-- f = 2 * height wi
--
-- >>> which is equal to the projection of wi to n
-- f = -2 * dot n wi
reflect ::
  -- | Normal
  V3 ('Direction 'Normalized) ->
  -- | Incoming direction (facing surface)
  V3 ('Direction 'Normalized) ->
  -- | Outgoing reflected direction
  V3 ('Direction 'Normalized)
reflect n wi = unsafeNormalized ((-2 * dot n wi) .*. n .+. wi)

-- | Compute the refraction vector
refract ::
  -- | IOR (ratio of inside IOR to outside IOR)
  Float ->
  -- | Normal
  V3 ('Direction 'Normalized) ->
  -- | Incoming direction (facing surface)
  V3 ('Direction 'Normalized) ->
  -- | Outgoing reflected direction as well as the transmission fresnel coef.
  Maybe (Float, V3 ('Direction 'Normalized))
refract ior' normal direction =
  let outside = dot direction normal < 0
      nl = if outside then normal else flipDirection normal

      -- Here we compute the refraction angle
      -- Do to fresnel laws, total internal reflection may happen, hence no refraction
      -- n_1 sin t_1 = n_2 sin t_2
      -- ior = n1 / n2
      ior = if outside then 1 / ior' else ior'

      cos_t1 = direction `dot` nl
      cos_t2_2 = 1 - ior * ior * (1 - cos_t1 * cos_t1)
   in if cos_t2_2 < 0
        then Nothing
        else
          let tdir = normalize ((direction .*. ior) .-. (nl .*. (cos_t1 * ior + sqrt cos_t2_2)))

              -- Schlick approximation of Fresnel coefficient for reflection coefficient
              r0 = ((ior - 1) / (ior + 1)) ^ (2 :: Int)

              -- cosTheta must be the angle toward the light
              -- I don't understand smallpt here which uses the following:
              cosTheta = 1 - if outside then - cos_t1 else abs $ tdir `dot` nl
              -- cosTheta = 1 + cos_t1
              reflectCoef = r0 + (1 - r0) * (cosTheta ^ (5 :: Int))
           in Just (1 - reflectCoef, tdir)
