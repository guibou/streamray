{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | This is the home for all the ray tracing function. It stores 'Ray', 'Object'
-- (which represents a 'Sphere' with a 'Material') as well as the different
-- raytracing functions.
module Streamray.Ray where

import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Streamray.Linear
import Streamray.Material

-- | This is a ray
-- Any point X on the ray can be represented using X = Origin + t * Direction.
data Ray = Ray
  { origin :: V3 'Position,
    direction :: V3 ('Direction 'Normalized)
  }
  deriving (Show)

-- | This is a Sphere
data Sphere = Sphere
  { center :: V3 'Position,
    radius :: Float
  }
  deriving (Show)

data Intersection = Intersection Object {-# UNPACK #-} !Float

-- | Returns the first intersection (if any) of a ray with a bunch of objets
rayIntersectObjets :: Ray -> [Object] -> Maybe Intersection
rayIntersectObjets ray = foldl' f Nothing
  where
    f Nothing obj@(Object _ sphere) = Intersection obj <$> rayIntersectSphere ray sphere
    f res@(Just (Intersection _ t)) obj'@(Object _ sphere) = case rayIntersectSphere ray sphere of
      Nothing -> res
      Just t'
        | t' < t -> Just (Intersection obj' t')
        | otherwise -> res

{-# INLINE rayIntersectSphere #-}
-- | Intersection between an object and sphere
rayIntersectSphere :: Ray -> Sphere -> Maybe Float
rayIntersectSphere Ray {origin, direction} Sphere {radius, center} =
  let -- >>> A point X on a ray is:
      --
      -- X = O + t * D
      --
      -- >>> A point X on a sphere is:
      --
      -- distance(X, C) = R
      --
      -- >>> We can inject X in the second equation:
      --
      -- distance(O + t * D, C) = R
      --
      -- >>> And replace distance(A, B) by length(A - B)
      --
      -- length(O + t * D - C) = R
      --
      -- >>> We can square both sides
      --
      -- length(t * D + O - C) ^ 2 = R ^ 2
      --
      -- >>> introduces some simplifications

      oc = origin --> center
      r2 = radius * radius
      -- length(t * D - OC) ^ 2 = r2
      --
      -- >>> let dot'(A) = dot(A, A) = length(A)^2
      --
      -- dot'(t * D - OC) = R2
      --
      -- >>> Distribute
      -- t ^ 2 * dot'(D) + dot'(OC) - 2 * t * dot(D, OC) = R2
      --
      -- >>> Let group terms with respect to t
      --
      -- t ^ 2 * dot'(D) +
      -- t * (-2 * dot(D, OC)) +
      -- dot'(OC) - R2
      -- = 0
      --
      -- >>> This is a second order equation which can be solved using
      -- >>> https://en.wikipedia.org/wiki/Quadratic_equation#Discriminant
      -- t ^ 2 * a + t * b + c = 0

      -- a = dot direction direction
      -- We know by construction that the ray direction is normalized, so a = 1

      -- b = - 2 * dot direction oc
      -- We simplify the next expressions by removing the division by 2
      -- We can also remove the negation of b
      b = dot direction oc
      c = dot oc oc - r2

      -- delta = b * b - 4 * a * c
      -- 4 and a are removed based on previous optimisations
      delta = b * b - c

      -- There is two solutions, t0 and t1, if delta >= 0
      sqrtDelta = sqrt delta
      t0 = b - sqrtDelta
      t1 = b + sqrtDelta
   in if
          | delta < 0 -> Nothing -- No solution, the ray missed the sphere
          | t0 >= 0 -> Just t0 -- t0 solution is the smallest (by construction), and is positive. The ray hit the front of the sphere.
          | t1 >= 0 -> Just t1 -- t1 solution is the smallest positive. The ray started inside the sphere and exits it.
          | otherwise -> Nothing -- neither t0 or t1 are positive, the ray is starting after the sphere.

-- | Represents a object, with its shape and material
data Object = Object Material Sphere
  deriving (Show)
