{-# LANGUAGE DataKinds #-}

-- | This module represents material, i.e. surface behaviors
module Streamray.Material where

import Streamray.Linear

-- | How the surface behaves with light.
data MaterialBehavior
  = -- | A diffuse material, which scatters light in every directions
    Diffuse
  | -- | A glass material, not used yet
    Glass
  | -- | A mirror material, not used yet
    Mirror
  deriving (Show)

-- | Material, with albedo and behavior
data Material
  = Material (V3 'Color) MaterialBehavior
  deriving (Show)
