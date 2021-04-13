{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | This is the home for all the ray tracing primitives.
module Streamray.Ray where

import Control.DeepSeq
import Data.Foldable (Foldable (toList), foldl')
import Data.Vector (Vector)
import GHC.Generics
import Streamray.Linear

-- | This is a ray
-- Any point X on the ray can be represented using X = Origin + t * Direction.
data Ray = Ray
  { origin :: V3 'Position,
    direction :: V3 ('Direction 'Normalized)
  }
  deriving (Show)

-- | This is a Sphere
data Sphere = Sphere
  { center :: {-# UNPACK #-} !(V3 'Position),
    radius :: {-# UNPACK #-} !Float
  }
  deriving (Show, Eq, NFData, Generic)

-- | Bounding volume hierarchy
data BVH t
  = BVHNode Box Box (BVH t) (BVH t)
  | BVHLeaf (Vector t)
  deriving (Show, NFData, Generic)

-- * Box

--

-- | Represents a box aligned with axis
data Box = Box {-# UNPACK #-} !(V3 'Position) {-# UNPACK #-} !(V3 'Position)
  deriving (Show, Eq, NFData, Generic)

data Axis = X | Y | Z deriving (Show, Eq)

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
instance HasBoundingBox Sphere where
  toBox (Sphere center radius) = Box (center .+. flipDirection dRadius) (center .+. dRadius)
    where
      dRadius = D radius radius radius

instance (HasBoundingBox t) => HasBoundingBox (Vector t) where
  toBox = makeBox

-- {-# INLINE makeBox #-}
-- {-# SPECIALISE makeBox :: Vector Sphere -> Box #-}

-- | Bounding box of all objects
makeBox :: Foldable f => HasBoundingBox t => f t -> Box
makeBox l = foldl' f (toBox x) xs
  where
    (x : xs) = toList l
    f box x = box <> toBox x

class HasBoundingBox t where
  toBox :: t -> Box

instance (HasBoundingBox t) => HasBoundingBox (BVH t) where
  toBox bvh = case bvh of
    BVHLeaf o -> toBox o
    BVHNode b0 b1 _ _ -> b0 <> b1

instance HasBoundingBox Box where
  toBox = id
