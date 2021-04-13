{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Represents a static scene, a cornel box with one light
module Streamray.Scene where

import Control.DeepSeq
import GHC.Generics
import Streamray.Light
import Streamray.Material
import Streamray.Ray
import Streamray.Geometry.Box

-- | This is a scene
data Scene = Scene
  { objects :: !SceneGraph,
    lights :: ![Light]
  }
  deriving (Show, Generic, NFData)

-- | Represents all the geometry in a scene
data SceneGraph
  = -- | That's all the objects in the scene
    SceneBVH !(BVH SceneGraph)
  | -- | A geometry with material
    AttachMaterial !Material !Geometry
  deriving (Show, Generic, NFData)

-- | This is an aggregate of primitives
data Geometry
  = Spheres !(BVH Sphere)
  | Boxes !(BVH Box)
  deriving (Show, Generic, NFData)

instance HasBoundingBox SceneGraph where
  toBox (SceneBVH bvh) = toBox bvh
  toBox (AttachMaterial _ o) = toBox o

instance HasBoundingBox Geometry where
  toBox (Spheres o) = toBox o
  toBox (Boxes o) = toBox o
