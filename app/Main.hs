{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Codec.Picture
import Data.List
import Data.Maybe
import Data.Ord (comparing)

import Streamray.Linear

-- | This is a ray
-- Any point X on the ray can be represented using X = Origin + t * Direction.
data Ray = Ray
  { origin :: V3 'Position,
    direction :: V3 ('Direction 'Normalized)
  }
  deriving (Show)

data Sphere = Sphere
  { center :: V3 'Position,
    radius :: Float
  }
  deriving (Show)

-- | Returns the first intersection (if any) of a ray with a bunch of objets
rayIntersectObjets :: Ray -> [Object] -> Maybe (Float, Object)
rayIntersectObjets ray objects = case intersections of
  [] -> Nothing
  l -> Just $ minimumBy (comparing fst) l
  where
    intersectionsMaybe = map (rayIntersectObject ray) objects
    intersections = catMaybes intersectionsMaybe

-- | Intersection between an object and ray
rayIntersectObject :: Ray -> Object -> Maybe (Float, Object)
rayIntersectObject ray o@(Object _ sphere) = case rayIntersectSphere ray sphere of
  Nothing -> Nothing
  Just t -> Just (t, o)

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
      b = - 2 * dot direction oc
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
sphereRadius :: Float
sphereRadius = 5000

scene :: [Object]
scene =
  [ Object
      (Material (C 1 1 0) Diffuse)
      (Sphere (P (sphereRadius + 500) 250 0) sphereRadius), -- Right
    Object
      (Material (C 0 1 1) Diffuse)
      (Sphere (P (- sphereRadius) 250 0) sphereRadius), -- Left
    Object
      (Material (C 1 1 1) Diffuse)
      (Sphere (P 250 (- sphereRadius) 0) sphereRadius), -- Top
    Object
      (Material (C 1 1 1) Diffuse)
      (Sphere (P 250 (sphereRadius + 500) 0) sphereRadius), -- Bottom
    Object
      (Material (C 1 1 1) Diffuse)
      (Sphere (P 250 250 (sphereRadius + 500)) sphereRadius), -- Back
      -- Small sphere 1
    Object
      (Material (C 1 1 1) Diffuse)
      (Sphere (P 150 350 350) 80),
    --
    -- Small sphere 1
    Object
      (Material (C 1 1 1) Diffuse)
      (Sphere (P 350 350 350) 80)
  ]

-- | Represents a object, with its shape and material
data Object = Object Material Sphere
  deriving (Show)

-- | Material, with albedo and behavior
data Material
  = Material (V3 'Color) MaterialBehavior
  deriving (Show)

data MaterialBehavior
  = -- | A diffuse material, which scatters light in every directions
    Diffuse
  | -- | A glass material, not used yet
    Glass
  | -- | A mirror material, not used yet
    Mirror
  deriving (Show)

lightPosition :: V3 'Position
lightPosition = P 250 250 250

lightEmission :: V3 'Color
lightEmission = C 30000 30000 30000

(-->) :: V3 'Position -> V3 'Position -> V3 ('Direction 'NotNormalized)
x --> y = y .-. x

-- | Returns the pixel color associated with a 'Ray'
radiance :: Ray -> PixelRGBA8
radiance ray = case rayIntersectObjets ray scene of
  Nothing -> PixelRGBA8 0 0 0 255
  Just (t, Object (Material albedo behavior) sphere) -> do
    let x = origin ray .+. t .*. direction ray
        directionToLight = x --> lightPosition
        normal = normalize (center sphere --> x)

        -- TODO: handle light distance and surface factors
        coef = dot normal (normalize directionToLight) / lightDistance2

        lightDistance2 = dot directionToLight directionToLight

    tonemap (lightEmission .*. (coef .*. albedo))

-- | Convert a light measure to a pixel value
tonemap :: V3 'Color -> PixelRGBA8
tonemap v = PixelRGBA8 x y z 255
  where
    -- truncate converts to Word8
    -- min/max clamps to the acceptable range
    -- pow (1 / 2.2) is doing gamma correction
    C (ftonemap -> x) (ftonemap -> y) (ftonemap -> z) = v

ftonemap :: Float -> Pixel8
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

main :: IO ()
main = writePng "first_image.png" $ generateImage raytrace 500 500
