{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module contains everything needed to intersect ray with primitives.
module Streamray.Intersect where

import Control.Monad.ST (runST)
import Control.Parallel (par, pseq)
import Data.Foldable
import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Algorithms.Tim as VSort
import Streamray.Linear
import Streamray.Ray

-- | Represents an intersection
data Intersection t = Intersection t {-# UNPACK #-} !Float

-- * BVH Internals

{-# INLINE sortVector #-}
sortVector :: Ord b => (a -> b) -> Vector a -> Vector a
sortVector f v = runST $ do
  v' <- Vector.thaw v
  VSort.sortBy (comparing f) v'
  Vector.unsafeFreeze v'

-- | Build a BVH from a list of Objects
buildBVH :: HasBoundingBox t => Vector t -> BVH t
buildBVH = buildBVH' 0

buildBVH' :: HasBoundingBox t => Int -> Vector t -> BVH t
buildBVH' !depth l
  | Vector.length l < 10 = BVHLeaf l
  | otherwise =
    if depth < 10
      then subA `par` subB `pseq` node
      else node
  where
    box = toBox l
    axis = boxBiggestAxis box

    l' = sortVector fSort l

    fSort (toBox -> Box (P x y z) _) = case axis of
      X -> x
      Y -> y
      Z -> z

    len = Vector.length l'

    (lA, lB) = Vector.splitAt (len `div` 2) l'

    subA = buildBVH' (depth + 1) lA
    subB = buildBVH' (depth + 1) lB

    boxA = toBox subA
    boxB = toBox subB
    node = BVHNode boxA boxB subA subB

-- * First intersection

{-# SPECIALIZE rayIntersectBVH :: Ray -> BVH Sphere -> Maybe (Intersection Sphere) #-}

-- | Returns the first intersection (if any) of a ray with a BVH
rayIntersectBVH :: Intersect (Vector t) => Ray -> BVH t -> Maybe (Intersection (IsObjectOrNot t))
rayIntersectBVH ray bvh = go bvh Nothing
  where
    go (BVHLeaf obs) currentIt = case rayIntersect ray obs of
      Nothing -> currentIt
      it'@(Just (Intersection _ t')) -> case currentIt of
        Nothing -> it'
        Just (Intersection _ t)
          | t' < t -> it'
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
      case (rayFirstOffsetInBox ray boxA, rayFirstOffsetInBox ray boxB) of
        (Nothing, Nothing) -> currentIt
        (Just tBoxA, Nothing) -> continue tBoxA subTreeA currentIt
        (Nothing, Just tBoxB) -> continue tBoxB subTreeB currentIt
        (Just tBoxA, Just tBoxB) ->
          let (firstTree, secondTree, firstT, secondT)
                | tBoxA <= tBoxB = (subTreeA, subTreeB, tBoxA, tBoxB)
                | otherwise = (subTreeB, subTreeA, tBoxB, tBoxA)
           in continue secondT secondTree (continue firstT firstTree currentIt)

{-# SPECIALIZE rayIntersectObjets :: Ray -> [Sphere] -> Maybe (Intersection Sphere) #-}
{-# SPECIALIZE rayIntersectObjets :: Ray -> [Object [Sphere]] -> Maybe (Intersection (Object Sphere)) #-}

-- | Returns the first intersection (if any) of a ray with a bunch of objets
rayIntersectObjets :: Foldable f => Intersect t => Ray -> f t -> Maybe (Intersection (IsObjectOrNot t))
rayIntersectObjets ray = foldl' f Nothing
  where
    f Nothing obj = rayIntersect ray obj
    f res@(Just (Intersection _ t)) obj' = case rayIntersect ray obj' of
      Nothing -> res
      it'@(Just (Intersection _ t'))
        | t' < t -> it'
        | otherwise -> res

-- * Box

-- Based on https://tavianator.com/fast-branchless-raybounding-box-intersections/

{-# INLINE rayFirstOffsetInBox #-}

-- | return the first positive offset of the ray which is in the box. It will
-- be 0 if the ray starts from inside the box.
rayFirstOffsetInBox :: Ray -> Box -> Maybe Float
rayFirstOffsetInBox ray box = case rayIntersectBoxRange ray box of
  Nothing -> Nothing
  Just (tmin, tmax)
    | tmin >= 0 -> Just tmin
    | tmax >= 0 -> Just 0
    | otherwise -> Nothing

{-# INLINE rayIntersectBox #-}

-- | returns the first (positive) intersection of the ray with the box
rayIntersectBox :: Ray -> Box -> Maybe Float
rayIntersectBox ray box = case rayIntersectBoxRange ray box of
  Nothing -> Nothing
  Just (tmin, tmax)
    | tmin >= 0 -> Just tmin
    | tmax >= 0 -> Just tmax
    | otherwise -> Nothing

{-# INLINE rayIntersectBoxRange #-}

-- | Returns the entry and exit point of ray/box intersection. That is,
-- @rayIntersectBoxRange ray box@ returns @Just (tmin, tmax)@ meaning that the
-- ray enter the box at @tmin@ and exit it at @tmax@, which can be negative.
rayIntersectBoxRange :: Ray -> Box -> Maybe (Float, Float)
rayIntersectBoxRange (Ray (P ox oy oz) (N dx dy dz)) (Box (P pminx pminy pminz) (P pmaxx pmaxy pmaxz))
  | tmax'' < tmin'' = Nothing
  | otherwise = Just (tmin'', tmax'')
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

-- * Visibility

-- | Test visibility with early exit for BVH
testRayVisibilityBVH :: Intersect t => Ray -> BVH t -> Float -> Bool
testRayVisibilityBVH ray (BVHLeaf obs) distance2 = testRayVisibility ray obs distance2
testRayVisibilityBVH ray (BVHNode boxA boxB subTreeA subTreeB) distance2 = visibleA && visibleB
  where
    itBoxA = case rayFirstOffsetInBox ray boxA of
      Nothing -> False
      Just _ -> True

    itBoxB = case rayFirstOffsetInBox ray boxB of
      Nothing -> False
      Just _ -> True

    visibleA = not itBoxA || testRayVisibilityBVH ray subTreeA distance2
    visibleB = not itBoxB || testRayVisibilityBVH ray subTreeB distance2

-- | Test visibility with early exit
testRayVisibilityObject :: (Intersect t, Foldable f) => Ray -> f t -> Float -> Bool
testRayVisibilityObject ray objects distance2 = foldr f True objects
  where
    f _ False = False
    f o True = testRayVisibility ray o distance2

-- * Sphere

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

-- | When computing an intersection, we need to know if the result is
-- associated with a material or not.
type family IsObjectOrNot t where
  IsObjectOrNot (Object t) = Object Sphere
  IsObjectOrNot [t] = IsObjectOrNot t
  IsObjectOrNot (BVH t) = IsObjectOrNot t
  IsObjectOrNot Sphere = Sphere
  IsObjectOrNot (Vector t) = IsObjectOrNot t

-- | Represents an object which can be intersected
class Intersect t where
  -- | Returns the closest intersection
  rayIntersect :: Ray -> t -> Maybe (Intersection (IsObjectOrNot t))
  -- | Returns True if the ray is not occluded by the object between the origin
  -- and the squared distance
  testRayVisibility :: Ray -> t -> Float -> Bool

-- | Intersect the content of an object and wrap it with the associated material
instance (IsObjectOrNot t ~ Sphere, Intersect t) => Intersect (Object t) where
  rayIntersect ray (Object m prims) =
    ( \(Intersection p t) ->
        Intersection (Object m p) t
    )
      <$> rayIntersect ray prims
  testRayVisibility ray (Object _ prims) distance2 = testRayVisibility ray prims distance2

instance Intersect Sphere where
  rayIntersect ray s = Intersection s <$> rayIntersectSphere ray s
  testRayVisibility ray s distance2 = case rayIntersectSphere ray s of
    Nothing -> True
    Just t
      | t * t < distance2 -> False
      | otherwise -> True

instance Intersect t => Intersect [t] where
  rayIntersect ray l = rayIntersectObjets ray l
  testRayVisibility ray l distance2 = testRayVisibilityObject ray l distance2

instance Intersect t => Intersect (Vector t) where
  rayIntersect ray l = rayIntersectObjets ray l
  testRayVisibility ray l distance2 = testRayVisibilityObject ray l distance2

instance (Intersect t) => Intersect (BVH t) where
  rayIntersect ray l = rayIntersectBVH ray l
  testRayVisibility ray l distance2 = testRayVisibilityBVH ray l distance2
