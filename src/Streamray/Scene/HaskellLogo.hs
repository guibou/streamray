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

{-# NOINLINE haskellLogoTriangles #-}
haskellLogoTriangles :: Vector Triangle
haskellLogoTriangles = unsafePerformIO (readTriangles "assets/haskell_logo.obj")

dTheta :: Float
dTheta = 2 * pi / 10

dPhi :: Float
dPhi = pi / 10

radius :: Float
radius = 250

bar :: Int -> Int -> Int
bar = undefined

haskellLogo :: Scene
haskellLogo = Scene objects lights
  where
    logo = AttachMaterial (Material (C 1 1 1) Diffuse (C 0 0 0)) (Triangles $ buildBVH haskellLogoTriangles)
    objects =
      Transformed (translate (D 250 250 500)) $
        SceneBVH $
          buildBVH $
            do
              phi <- [0, dTheta .. 2 * pi]
              theta <- [0, dPhi .. pi]

              let s = 10 * cos (pi / 2 - theta)

              pure $
                Transformed
                  ( rotate (N 0 1 0) phi
                      `composeTransform` rotate (N 1 0 0) (pi / 2 - theta)
                      `composeTransform` translate (D 0 0 (- radius))
                      `composeTransform` scale (D s s s)
                  )
                  logo

    lights =
      [ Light (PointLight (P 50 250 250)) (C 30000 10000 10000),
        Light (PointLight (P 450 250 250)) (C 10000 10000 30000),
        Light (PointLight (P 250 250 0)) (C 30000 30000 30000)
      ]
