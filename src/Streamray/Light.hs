{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module represents lights
module Streamray.Light where

import Control.DeepSeq
import GHC.Generics
import Streamray.Linear
import Streamray.Ray

-- | This is a light, with its behavior and an emission.
data Light = Light
  { behavior :: LightBehavior,
    emission :: V3 'Color
  }
  deriving (Show, Eq, NFData, Generic)

-- | The shape of the light
data LightBehavior
  = -- | A simple point in space
    PointLight (V3 'Position)
  | -- | A sphere
    SphereLight Sphere
  deriving (Show, Eq, NFData, Generic)
