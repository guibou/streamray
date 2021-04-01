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

-- | Represents a object, with its shape and material
data Object = Object Material Sphere
  deriving (Show)
