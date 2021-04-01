{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | This is the core of the rendering algorithm, with the main raytrace
-- "integrator", called 'radiance', as well as a naive camera model and image
-- saving.
module Streamray.Render where

import Codec.Picture
import Streamray.Linear
import Streamray.Material
import Streamray.Ray
import Streamray.Scene

-- | Returns the pixel color associated with a 'Ray'
radiance :: Ray -> PixelRGBA8
radiance ray = case rayIntersectObjets ray scene of
  Nothing -> PixelRGBA8 0 0 0 255
  Just (t, Object (Material albedo behavior) sphere) -> do
    let x = origin ray .+. t .*. direction ray
        directionToLight = x --> lightPosition
        normal = normalize (center sphere --> x)

        -- TODO: handle surface factors
        directionToLightNormalized = normalize directionToLight
        coef = max 0 (dot normal directionToLightNormalized / lightDistance2)

        lightDistance2 = dot directionToLight directionToLight

        -- Trace a ray toward the light source and check for intersection
        canSeeLightSource = case rayIntersectObjets
          ( Ray
              -- The origin of the ray is biased toward the light to avoid self
              -- shadows if the point is slightly under the surface due to
              -- floating point approximations.
              ( x
                  .+. epsilon .*. directionToLightNormalized
              )
              directionToLightNormalized
          )
          scene of
          -- No intersection, we see the light
          Nothing -> True
          -- There is an intersection, we check that it happen "AFTER" the light.
          Just (tIntersect, _) -> tIntersect ** 2 > lightDistance2

        visibility = if canSeeLightSource then C 1 1 1 else C 0 0 0

    tonemap (visibility .*. lightEmission .*. (coef .*. albedo))

-- | Changes the ray offset used to escape surface
epsilon :: Float
epsilon = 0.01

-- | Convert a light measure to a pixel value
tonemap :: V3 'Color -> PixelRGBA8
tonemap v = PixelRGBA8 x y z 255
  where
    -- truncate converts to Word8
    -- min/max clamps to the acceptable range
    -- pow (1 / 2.2) is doing gamma correction
    C (ftonemap -> x) (ftonemap -> y) (ftonemap -> z) = v

-- | Converts a value from a measure to the RGB colorspace in 8bit.
ftonemap :: Float -> Pixel8
-- truncate converts to Word8
-- min/max clamps to the acceptable range
-- pow (1 / 2.2) is doing gamma correction
ftonemap = truncate @Float @Pixel8 . max 0 . min 255 . (* 255) . (** (1 / 2.2))

-- | Raytrace a 500x500 image
-- This function is called for each pixel
raytrace :: Int -> Int -> PixelRGBA8
raytrace (fromIntegral -> x) (fromIntegral -> y) = radiance ray
  where
    -- Generate a ray in the XY plane and pointing in the Z direction
    coefOpening = 1.001

    n = P x y 0

    -- n' is on the plane [-250:250]
    n'@(P x' y' 0) = n .-. D 250 250 0
    f = P (coefOpening * x') (coefOpening * y') 1

    d = normalize (n' --> f)
    ray = Ray n d

-- | Raytrace a 500x500 image, using the default scene, and saves it.
raytraceImage :: FilePath -> IO ()
raytraceImage path = writePng path $ generateImage raytrace 500 500
