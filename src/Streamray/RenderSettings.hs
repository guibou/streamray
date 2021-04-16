module Streamray.RenderSettings where

import Streamray.Scene
import Streamray.Scene.Cornel
import Streamray.Scene.HaskellLogo
import Streamray.Scene.ManySpheres
import Streamray.Scene.SphereSampling

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
  HaskellLogo -> haskellLogo
  SphereSampling -> sphereSampling

data KnownScene
  = Cornel
  | ManySpheres Float
  | HaskellLogo
  | SphereSampling
  deriving (Show, Read)
