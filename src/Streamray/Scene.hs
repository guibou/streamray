{-# LANGUAGE DataKinds #-}

-- | Represents a static scene, a cornel box with one light
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Streamray.Scene where

import Streamray.Ray
import Streamray.Light
import Streamray.Intersect
import Data.Maybe (mapMaybe)
import Control.DeepSeq
import GHC.Generics

-- TODO: it should be vector instead of list
-- | This is a scene
data Scene = Scene
  { objects :: BVH,
    lights :: [Light]
  }
  deriving (Show, Generic, NFData)

mkScene :: [Object] -> [Light] -> Scene
mkScene objects lights =
  Scene
    (buildBVH objects)
    (lights ++ mapMaybe objectToLight objects)
