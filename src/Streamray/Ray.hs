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
import Data.Vector (Vector)
import GHC.Generics
import Streamray.Geometry.Box
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

-- | Returns the bounding box of a sphere
instance HasBoundingBox Sphere where
  toBox (Sphere center radius) = Box (center .+. flipDirection dRadius) (center .+. dRadius)
    where
      dRadius = D radius radius radius

instance (HasBoundingBox t) => HasBoundingBox (BVH t) where
  toBox bvh = case bvh of
    BVHLeaf o -> toBox o
    BVHNode b0 b1 _ _ -> b0 <> b1
