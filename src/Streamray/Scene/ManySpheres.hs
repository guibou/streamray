{-# LANGUAGE ViewPatterns #-}
module Streamray.Scene.ManySpheres where

import Streamray.Light
import Streamray.Linear
import Streamray.Material
import Streamray.Ray
import Streamray.Scene

white :: Material
white = Material (C 1 1 1) Diffuse (C 0 0 0)

manySpheres :: Int -> Scene
manySpheres (fromIntegral -> dd) = mkScene objects lights
  where
    objects = do
      x <- [0, dd .. 500]
      y <- [0, dd .. 500]
      z <- [0, dd .. 500]

      pure $ Object white (Sphere (P x y z) 10)

    lights =
      [ Light (PointLight (P 50 250 250)) (C 30000 10000 10000),
        Light (PointLight (P 450 250 250)) (C 10000 10000 30000)
      ]
