{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

module Streamray.Scene.ManySpheres where

import qualified Data.Vector as Vector
import Streamray.Geometry.Box
import Streamray.Intersect
import Streamray.Light
import Streamray.Linear
import Streamray.Material
import Streamray.Ray
import Streamray.Scene

white :: Material
white = Material (C 1 1 1) Diffuse Nothing

manySpheres :: Float -> Scene
manySpheres dd = Scene objects lights
  where
    objects =
      SceneBVH
        ( buildBVH
            [ AttachMaterial white (Spheres (buildBVH sSpheres)),
              AttachMaterial white (Boxes (buildBVH sBox))
            ]
        )

    sSpheres = Vector.fromList $ do
      x <- [0, dd .. 250]
      y <- [0, dd .. 500]
      z <- [0, dd .. 500]

      pure $ Sphere (P x y z) 10

    sBox = Vector.fromList $ do
      x <- [250, 250 + dd .. 500]
      y <- [0, dd .. 500]
      z <- [0, dd .. 500]

      pure $ toBox $ Sphere (P x y z) 10

    lights =
      [ Light (PointLight (P 50 250 250)) (C 30000 10000 10000),
        Light (PointLight (P 450 250 250)) (C 10000 10000 30000)
      ]
