{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Streamray.Geometry.Box where

import Control.DeepSeq
import Data.Foldable
import Data.Vector (Vector)
import GHC.Generics
import Streamray.Linear

-- | Represents a box aligned with axis
data Box = Box {-# UNPACK #-} !(V3 'Position) {-# UNPACK #-} !(V3 'Position)
  deriving (Show, Eq, NFData, Generic)

-- | This is the union of box
instance Semigroup Box where
  Box pMin pMax <> Box pMin' pMax' =
    Box (minP pMin pMin') (maxP pMax pMax')

minP :: V3 'Position -> V3 'Position -> V3 'Position
minP (P x y z) (P x' y' z') = P (min x x') (min y y') (min z z')

maxP :: V3 'Position -> V3 'Position -> V3 'Position
maxP (P x y z) (P x' y' z') = P (max x x') (max y y') (max z z')

instance Monoid Box where
  mempty = Box (P inf inf inf) (P minf minf minf)
    where
      inf = 1 / 0
      minf = - inf

class HasBoundingBox t where
  toBox :: t -> Box

instance HasBoundingBox Box where
  toBox = id

instance (HasBoundingBox t) => HasBoundingBox (Vector t) where
  toBox = makeBox

-- {-# INLINE makeBox #-}

-- | Bounding box of all objects
makeBox :: (Foldable f, HasBoundingBox t) => f t -> Box
makeBox = foldl' f mempty
  where
    f box x = box <> toBox x

-- | Return the largest axis of a box
boxBiggestAxis :: Box -> Axis
boxBiggestAxis (Box pMin pMax)
  | x >= y && x >= z = X
  | y >= z = Y
  | otherwise = Z
  where
    D x y z = pMin --> pMax

data Axis = X | Y | Z deriving (Show, Eq)

-- Build a box from two points
buildBox :: V3 'Position -> V3 'Position -> Box
buildBox (P x y z) (P x' y' z') = Box (P (min x x') (min y y') (min z z')) (P (max x x') (max y y') (max z z'))

-- Add a point to a box
addPointToBox :: Box -> V3 'Position -> Box
addPointToBox (Box pMin pMax) p = Box (minP pMin p) (maxP pMax p)
