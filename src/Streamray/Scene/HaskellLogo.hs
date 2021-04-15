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
    objects = AttachMaterial (Material (C 1 1 1) Diffuse (C 0 0 0)) (Triangles $ buildBVH $ fmap transform haskellLogoTriangles)

    lights =
      [ Light (PointLight (P 50 250 250)) (C 30000 10000 10000),
        Light (PointLight (P 450 250 250)) (C 10000 10000 30000),
        Light (PointLight (P 250 250 250)) (C 30000 30000 30000)
      ]

-- Resize and scale the asset
transform :: Triangle -> Triangle
transform (Triangle p0 p1 p2) = Triangle (t p0) (t p1) (t p2)
  where
    t (P x y z) = P (t' x) (t' y) (t' z)
    t' c = 60 * c + 250
