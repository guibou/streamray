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

data Box = Box (V3 'Position) (V3 'Position)
  deriving (Show)

instance Semigroup Box where
  Box pMin pMax <> Box pMin' pMax' =
    Box (minP pMin pMin') (maxP pMax pMax')
    where
      minP (P x y z) (P x' y' z') = P (min x x') (min y y') (min z z')
      maxP (P x y z) (P x' y' z') = P (max x x') (max y y') (max z z')

data Axis = X | Y | Z
  deriving (Show)

boxBiggestAxis :: Box -> Axis
boxBiggestAxis (Box pMin pMax)
  | x >= y && x >= z = X
  | y >= z = Y
  | otherwise = Z
  where
    D x y z = pMin --> pMax

data BVH
  = BVHNode Box Box BVH BVH
  | BVHLeaf Object
  deriving (Show)

buildBVH :: [Object] -> BVH
buildBVH [] = error "it should not happen"
buildBVH [x] = BVHLeaf x
buildBVH l = BVHNode boxA boxB subA subB
  where
    box = makeBox l
    axis = boxBiggestAxis box

    l' = sortOn fSort l

    fSort (Object _ (Sphere (P x y z) _)) = case axis of
      X -> x
      Y -> y
      Z -> z

    len = length l'

    (lA, lB) = splitAt (len `div` 2) l'
    boxA = makeBox lA
    boxB = makeBox lB

    subA = buildBVH lA
    subB = buildBVH lB

makeBox :: [Object] -> Box
makeBox [] = error "it should not happen 2"
makeBox (Object _ sphere : xs) = foldl' f (sphereToBox sphere) xs
  where
    f box (Object _ sphere') = box <> sphereToBox sphere'

rayIntersectBVH :: Ray -> BVH -> Maybe Intersection
rayIntersectBVH ray bvh = go bvh Nothing
  where
    go (BVHLeaf o@(Object _ sphere)) currentIt = case rayIntersectSphere ray sphere of
      Nothing -> currentIt
      Just t' -> case currentIt of
        Nothing -> Just $ Intersection o t'
        Just (Intersection _ t)
          | t' < t -> Just $ Intersection o t'
          | otherwise -> currentIt
    go (BVHNode boxA boxB subTreeA subTreeB) currentIt = do
        -- Continue walking the subtree only if the box intersection is closer
        -- than the current intersection.
      let continue tBox sub currentIt' =
            let currentItIsCloser = case currentIt' of
                  Nothing -> False
                  Just (Intersection _ tCurrent) -> tCurrent <= tBox
             in if currentItIsCloser
                  then currentIt'
                  else go sub currentIt'
      case (rayIntersectBox ray boxA, rayIntersectBox ray boxB) of
        (Nothing, Nothing) -> currentIt
        (Just tBoxA, Nothing) -> continue tBoxA subTreeA currentIt
        (Nothing, Just tBoxB) -> continue tBoxB subTreeB currentIt
        (Just tBoxA, Just tBoxB) ->
          let (firstTree, secondTree, firstT, secondT)
                | tBoxA <= tBoxB = (subTreeA, subTreeB, tBoxA, tBoxB)
                | otherwise = (subTreeB, subTreeA, tBoxB, tBoxA)
           in continue secondT secondTree (continue firstT firstTree currentIt)

{-# INLINE rayIntersectBox #-}

-- Based on https://tavianator.com/fast-branchless-raybounding-box-intersections/

-- | return 'True' if 'Ray' intersects 'Box'
rayIntersectBox :: Ray -> Box -> Maybe Float
rayIntersectBox (Ray (P ox oy oz) (N dx dy dz)) (Box (P pminx pminy pminz) (P pmaxx pmaxy pmaxz))
  | tmax'' < tmin'' = Nothing
  | tmin'' >= 0 = Just tmin''
  | tmax'' >= 0 = Just 0
  | otherwise = Nothing
  where
    rinvx = 1 / dx
    rinvy = 1 / dy
    rinvz = 1 / dz

    -- X slab
    tx1 = (pminx - ox) * rinvx
    tx2 = (pmaxx - ox) * rinvx

    tmin = min tx1 tx2
    tmax = max tx1 tx2

    -- Y slab
    ty1 = (pminy - oy) * rinvy
    ty2 = (pmaxy - oy) * rinvy

    tmin' = max tmin $ min ty1 ty2
    tmax' = min tmax $ max ty1 ty2

    -- Z slab
    tz1 = (pminz - oz) * rinvz
    tz2 = (pmaxz - oz) * rinvz

    tmin'' = max tmin' $ min tz1 tz2
    tmax'' = min tmax' $ max tz1 tz2

-- | Test visibility with early exit
testRayVisibilityBVH :: Ray -> BVH -> Float -> Bool
testRayVisibilityBVH ray (BVHLeaf (Object _ sphere)) distance2 = case rayIntersectSphere ray sphere of
  Nothing -> True
  Just t -> t * t >= distance2
testRayVisibilityBVH ray (BVHNode boxA boxB subTreeA subTreeB) distance2 = visibleA && visibleB
  where
    itBoxA = case rayIntersectBox ray boxA of
      Nothing -> False
      Just _ -> True

    itBoxB = case rayIntersectBox ray boxB of
      Nothing -> False
      Just _ -> True

    visibleA = not itBoxA || testRayVisibilityBVH ray subTreeA distance2
    visibleB = not itBoxB || testRayVisibilityBVH ray subTreeB distance2

-- | Test visibility with early exit
testRayVisibility :: Ray -> [Object] -> Float -> Bool
testRayVisibility ray objects distance2 = go objects
  where
    go [] = True
    go (Object _ sphere : xs) = case rayIntersectSphere ray sphere of
      Nothing -> go xs
      Just t
        | t * t < distance2 -> False
        | otherwise -> go xs

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

sphereToBox :: Sphere -> Box
sphereToBox (Sphere center radius) = Box (center .+. flipDirection dRadius) (center .+. dRadius)
  where
    dRadius = D radius radius radius

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
          -- TODO: check if removing this test is good for performances
          | delta < 0 -> Nothing -- No solution, the ray missed the sphere
          | t0 >= 0 -> Just t0 -- t0 solution is the smallest (by construction), and is positive. The ray hit the front of the sphere.
          | t1 >= 0 -> Just t1 -- t1 solution is the smallest positive. The ray started inside the sphere and exits it.
          | otherwise -> Nothing -- neither t0 or t1 are positive, the ray is starting after the sphere.

-- | Represents a object, with its shape and material
data Object = Object Material Sphere
  deriving (Show)
