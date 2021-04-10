{-# LANGUAGE DataKinds #-}

-- | Represents a static scene, a cornel box with one light
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Streamray.Scene where

import Streamray.Ray
import Streamray.Light
import Streamray.Intersect
import Data.Vector (Vector)
import Control.DeepSeq
import GHC.Generics

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
