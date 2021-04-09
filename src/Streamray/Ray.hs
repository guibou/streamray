{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | This is the home for all the ray tracing primitives.
module Streamray.Ray where

import Data.Foldable (foldl')
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

-- | Represents a object, with its shape and material
data Object = Object Material Sphere
  deriving (Show)

-- | Bounding volume hierarchy
data BVH
  = BVHNode Box Box BVH BVH
  | BVHLeaf Object
  deriving (Show)

-- * Box

--

-- | Represents a box aligned with axis
data Box = Box (V3 'Position) (V3 'Position)
  deriving (Show)

-- | An Axis
data Axis = X | Y | Z
  deriving (Show)

-- | Return the largest axis of a box
boxBiggestAxis :: Box -> Axis
boxBiggestAxis (Box pMin pMax)
  | x >= y && x >= z = X
  | y >= z = Y
  | otherwise = Z
  where
    D x y z = pMin --> pMax

-- | This is the union of box
instance Semigroup Box where
  Box pMin pMax <> Box pMin' pMax' =
    Box (minP pMin pMin') (maxP pMax pMax')
    where
      minP (P x y z) (P x' y' z') = P (min x x') (min y y') (min z z')
      maxP (P x y z) (P x' y' z') = P (max x x') (max y y') (max z z')

-- | Returns the bounding box of a sphere
sphereToBox :: Sphere -> Box
sphereToBox (Sphere center radius) = Box (center .+. flipDirection dRadius) (center .+. dRadius)
  where
    dRadius = D radius radius radius

-- | Bounding box of all objects
makeBox :: [Object] -> Box
makeBox [] = error "it should not happen 2"
makeBox (Object _ sphere : xs) = foldl' f (sphereToBox sphere) xs
  where
    f box (Object _ sphere') = box <> sphereToBox sphere'
