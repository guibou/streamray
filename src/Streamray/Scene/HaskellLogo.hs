{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

module Streamray.Scene.HaskellLogo where

import Data.Vector (Vector)
import GHC.IO (unsafePerformIO)
import Streamray.Geometry.Triangle
import Streamray.Intersect
import Streamray.Light
import Streamray.Linear
import Streamray.Material
import Streamray.Scene

haskellLogoTriangles :: Vector Triangle
haskellLogoTriangles = unsafePerformIO (readTriangles "assets/haskell_logo.obj")

haskellLogo :: Scene
haskellLogo = Scene objects lights
  where
    logo = AttachMaterial (Material (C 1 1 1) Diffuse (C 0 0 0)) (Triangles $ buildBVH haskellLogoTriangles)
    objects =
      SceneBVH $
        buildBVH
          [ Transformed (translate (D 450 450 450) `composeTransform` scale (D 40 40 40) `composeTransform` rotateZ (pi / 2)) logo,
            Transformed (translate (D 50 450 450) `composeTransform` scale (D 30 30 30) `composeTransform` rotateY (pi / 2)) logo,
            Transformed (translate (D 50 50 450) `composeTransform` scale (D 20 20 20) `composeTransform` rotateX (pi / 2)) logo,
            Transformed (translate (D 450 50 450) `composeTransform` scale (D 10 10 10) `composeTransform` rotateX (pi / 2)) logo
          ]

    lights =
      [ Light (PointLight (P 50 250 250)) (C 30000 10000 10000),
        Light (PointLight (P 450 250 250)) (C 10000 10000 30000),
        Light (PointLight (P 250 250 250)) (C 30000 30000 30000)
      ]
