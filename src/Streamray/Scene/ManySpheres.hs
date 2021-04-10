{-# LANGUAGE ViewPatterns #-}

module Streamray.Scene.ManySpheres where

import qualified Data.Vector as Vector
import Streamray.Light
import Streamray.Linear
import Streamray.Material
import Streamray.Ray
import Streamray.Scene

white :: Material
white = Material (C 1 1 1) Diffuse (C 0 0 0)

manySpheres :: Float -> Scene
manySpheres dd = mkScene (Vector.singleton objects) lights
  where
    objects = Object white ss
    ss = Vector.fromList $ do
      x <- [0, dd .. 500]
      y <- [0, dd .. 500]
      z <- [0, dd .. 500]

      pure $ Sphere (P x y z) 1

    lights =
      [ Light (PointLight (P 50 250 250)) (C 30000 10000 10000),
        Light (PointLight (P 450 250 250)) (C 10000 10000 30000)
      ]
