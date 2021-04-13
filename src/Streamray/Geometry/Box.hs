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
    where
      minP (P x y z) (P x' y' z') = P (min x x') (min y y') (min z z')
      maxP (P x y z) (P x' y' z') = P (max x x') (max y y') (max z z')

class HasBoundingBox t where
  toBox :: t -> Box

instance HasBoundingBox Box where
  toBox = id

instance (HasBoundingBox t) => HasBoundingBox (Vector t) where
  toBox = makeBox

-- {-# INLINE makeBox #-}

-- | Bounding box of all objects
makeBox :: (Foldable f, HasBoundingBox t) => f t -> Box
makeBox l = foldl' f (toBox x) xs
  where
    (x : xs) = toList l
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
