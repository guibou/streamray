{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Streamray.Scene.MIS where

import Streamray.Geometry.Triangle
import Streamray.Intersect
import Streamray.Light
import Streamray.Linear
import Streamray.Material
import Streamray.Ray (Sphere (Sphere))
import Streamray.Scene

mis :: Scene
mis = Scene objects lights
  where
    plane =
      buildBVH
        [ Triangle (P 0 0 (-40)) (P 0 0 40) (P 500 0 (-40)),
          Triangle (P 500 0 (-40)) (P 0 0 40) (P 500 0 40)
        ]
    objects =
      SceneBVH $
        buildBVH
          [ Transformed
              ( translate (D 0 100 100)
                  `composeTransform` rotate (N 1 0 0) (1.25 * pi / 2)
              )
              $ object (Material white (Glossy 0) black) plane,
            Transformed
              ( translate (D 0 200 100)
                  `composeTransform` rotate (N 1 0 0) (1.1 * pi / 2)
              )
              $ object (Material white (Glossy 50) black) plane,
            Transformed
              ( translate (D 0 300 100)
                  `composeTransform` rotate (N 1 0 0) ((1 / 1.1) * pi / 2)
              )
              $ object (Material white (Glossy 1000) black) plane,
            Transformed
              ( translate (D 0 400 100)
                  `composeTransform` rotate (N 1 0 0) ((1 / 1.35) * pi / 2)
              )
              $ object (Material white Mirror black) plane,
              aLight (P 50 250 (-100)) 70 (C lScale 0 0),
              aLight (P 150 250 (-100)) 50 (C 0 lScale 0),
              aLight (P 250 250 (-100)) 30 (C 0 0 lScale),
              aLight (P 350 250 (-100)) 10 (C lScale lScale 0)
          ]

    lights =
      [ -- Light (PointLight (P 450 250 (-100))) (C lScale lScale lScale),
        Light (SphereLight (Sphere (P 50 250 (-100)) 70)) (C lScale 0 0),
        Light (SphereLight (Sphere (P 150 250 (-100)) 50)) (C 0 lScale 0),
        Light (SphereLight (Sphere (P 250 250 (-100)) 30)) (C 0 0 lScale),
        Light (SphereLight (Sphere (P 350 250 (-100)) 10)) (C lScale lScale 0)
      ]

    aLight p r emit = AttachMaterial (Material black Diffuse (emit .*. (1 / (4 * pi * r * r)))) (Spheres (buildBVH [Sphere p r]))

    lScale = 10000

    black = C 0 0 0
    white = C 1 1 1

    object m s = AttachMaterial m (Triangles s)
