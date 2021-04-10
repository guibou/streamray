{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a static scene, a cornel box with one light
module Streamray.Scene where

import Control.DeepSeq
import Data.Vector (Vector)
import GHC.Generics
import Streamray.Intersect
import Streamray.Light
import Streamray.Ray

-- TODO: it should be vector instead of list

-- | This is a scene
data Scene = Scene
  { objects :: !(BVH (Object (BVH Sphere))),
    lights :: ![Light]
  }
  deriving (Show, Generic, NFData)

mkScene :: Vector (Object (Vector Sphere)) -> [Light] -> Scene
mkScene objects lights =
  Scene
    (makeRecursiveBVH objects)
    (lights ++ concatMap objectToLight objects)

makeRecursiveBVH :: HasBoundingBox t => Vector (Object (Vector t)) -> BVH (Object (BVH t))
makeRecursiveBVH l = buildBVH $ fmap (\(Object m prims) -> Object m (buildBVH prims)) l
