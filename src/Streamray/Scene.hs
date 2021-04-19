{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Represents a static scene, a cornel box with one light
module Streamray.Scene where

import Control.DeepSeq
import Data.Foldable
import GHC.Generics
import Streamray.Geometry.Box
import Streamray.Geometry.Triangle
import Streamray.Light
import Streamray.Linear
import Streamray.Material
import Streamray.Ray

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
  | Transformed Transform SceneGraph
  deriving (Show, Generic, NFData)

-- | This is an aggregate of primitives
data Geometry
  = Spheres !(BVH Sphere)
  | Boxes !(BVH Box)
  | Triangles !(BVH Triangle)
  deriving (Show, Generic, NFData)

instance HasBoundingBox SceneGraph where
  toBox (SceneBVH bvh) = toBox bvh
  toBox (AttachMaterial _ o) = toBox o
  toBox (Transformed mat o) =
    let Box (P x y z) (P x' y' z') = toBox o
     in foldl' addPointToBox mempty $ do
          newX <- [x, x']
          newY <- [y, y']
          newZ <- [z, z']

          pure $ transformPoint mat (P newX newY newZ)

instance HasBoundingBox Geometry where
  toBox (Spheres o) = toBox o
  toBox (Boxes o) = toBox o
  toBox (Triangles o) = toBox o
