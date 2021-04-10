module Streamray.RenderSettings where

import Streamray.Scene
import Streamray.Scene.Cornel
import Streamray.Scene.ManySpheres

data RenderSettings = RenderSettings
  { samplesPerPixel :: Int,
    knownScene :: KnownScene,
    filepath :: FilePath
  }
  deriving (Show)

loadKnownScene :: RenderSettings -> Scene
loadKnownScene renderSettings = case knownScene renderSettings of
  Cornel -> cornel
  ManySpheres i -> manySpheres i

data KnownScene
  = Cornel
  | ManySpheres Float
  deriving (Show, Read)

