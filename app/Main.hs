{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Codec.Picture
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Linear

-- | This is a ray
-- Any point X on the ray can be represented using X = Origin + t * Direction.
data Ray = Ray
  { origin :: V3 Float,
    direction :: V3 Float
  }
  deriving (Show)

data Sphere = Sphere
  { center :: V3 Float,
    radius :: Float
  }
  deriving (Show)

-- | Returns the first intersection (if any) of a ray with a bunch of spheres.
rayIntersectSpheres :: Ray -> [Sphere] -> Maybe (Float, Sphere)
rayIntersectSpheres ray spheres = case intersections of
  [] -> Nothing
  l -> Just $ minimumBy (comparing fst) l
  where
    intersectionsMaybe :: [Maybe (Float, Sphere)]
    intersectionsMaybe = map (\s -> (,s) <$> rayIntersectSphere ray s) spheres
    intersections = catMaybes intersectionsMaybe

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

      oc = center - origin
      r2 = radius ** 2
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

      a = dot direction direction
      b = -2 * dot direction oc
      c = dot oc oc - r2

      delta = b ** 2 - 4 * a * c

      -- There is two solutions, t0 and t1, if delta >= 0
      t0 = (- b - sqrt delta) / (2 * a)
      t1 = (- b + sqrt delta) / (2 * a)
   in if
          | delta < 0 -> Nothing -- No solution, the ray missed the sphere
          | t0 >= 0 -> Just t0 -- t0 solution is the smallest (by construction), and is positive. The ray hit the front of the sphere.
          | t1 >= 0 -> Just t1 -- t1 solution is the smallest positive. The ray started inside the sphere and exits it.
          | otherwise -> Nothing -- neither t0 or t1 are positive, the ray is starting after the sphere.

-- | Initial scene
scene :: [Sphere]
scene =
  [ Sphere (V3 255 255 300) 200,
    Sphere (V3 0 255 300) 100
  ]

lightPosition :: V3 Float
lightPosition = V3 255 (-1000) 255

(-->) :: Num a => a -> a -> a
x --> y = y - x

-- | Returns the pixel color associated with a 'Ray'
radiance :: Ray -> PixelRGBA8
radiance ray = case rayIntersectSpheres ray scene of
  Nothing -> PixelRGBA8 255 0 0 255
  Just (t, sphere) -> do
    let x = origin ray + t *^ direction ray
        directionToLight = normalize (x --> lightPosition)
        normal = normalize (center sphere --> x)

        -- TODO: handle light distance and surface factors
        coef = dot normal directionToLight

        color = V3 coef coef coef

    tonemap color

{-
   coef_lumineux = dot normal directionToTheLight

-}
tonemap :: V3 Float -> PixelRGBA8
tonemap v =
  let V3 x y z = truncate . max 0 . min 255 <$> (v * 255)
   in PixelRGBA8 x y z 255

-- | Raytrace a 500x500 image
-- This function is called for each pixel
raytrace :: Int -> Int -> PixelRGBA8
raytrace x y = radiance ray
  where
    -- Generate a ray in the XY plane and pointing in the Z direction
    ray = Ray (V3 (fromIntegral x) (fromIntegral y) 0) (V3 0 0 1)

main :: IO ()
main = writePng "first_image.png" $ generateImage raytrace 500 500
