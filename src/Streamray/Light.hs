{-# LANGUAGE DataKinds #-}

-- | This module represents lights
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Streamray.Light where

import Streamray.Linear
import Streamray.Material
import Streamray.Ray
import Data.Foldable
import Control.DeepSeq
import GHC.Generics

-- | This is a light, with its behavior and an emission.
data Light = Light
  { behavior :: LightBehavior,
    emission :: V3 'Color
  }
  deriving (Show, Eq, NFData, Generic)

-- | The shape of the light
data LightBehavior
  -- | A simple point in space
  = PointLight (V3 'Position)
  -- | A sphere
  | SphereLight Sphere
  deriving (Show, Eq, NFData, Generic)

-- | Converts an object with possibly an emission to a light.
objectToLight :: Foldable f => Object (f Sphere) -> [Light]
objectToLight (Object (Material _ _ emit) f) = case emit of
  C 0 0 0 -> []
  _ -> do
       s <- toList f
       pure $ Light (SphereLight s) emit
