{-# LANGUAGE DataKinds #-}

module Streamray.Light where

import Streamray.Linear
import Streamray.Material
import Streamray.Ray

data Light = Light
  { behavior :: LightBehavior,
    emission :: V3 'Color
  }
  deriving (Show)

data LightBehavior
  = PointLight (V3 'Position)
  | SphereLight Sphere
  deriving (Show)

objectToLight :: Object -> Maybe Light
objectToLight (Object (Material _ _ emit) o) = case emit of
  C 0 0 0 -> Nothing
  _ -> Just (Light (SphereLight o) emit)
