{-# LANGUAGE DataKinds #-}

-- | This module represents lights
module Streamray.Light where

import Streamray.Linear
import Streamray.Material
import Streamray.Ray

-- | This is a light, with its behavior and an emission.
data Light = Light
  { behavior :: LightBehavior,
    emission :: V3 'Color
  }
  deriving (Show)

-- | The shape of the light
data LightBehavior
  -- | A simple point in space
  = PointLight (V3 'Position)
  -- | A sphere
  | SphereLight Sphere
  deriving (Show)

-- | Converts an object with possibly an emission to a light.
objectToLight :: Object -> Maybe Light
objectToLight (Object (Material _ _ emit) o) = case emit of
  C 0 0 0 -> Nothing
  _ -> Just (Light (SphereLight o) emit)
