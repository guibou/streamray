{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

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
              $ object (Material white Mirror black) plane,
            Transformed
              ( translate (D 0 200 100)
                  `composeTransform` rotate (N 1 0 0) (1.1 * pi / 2)
              )
              $ object (Material white Mirror black) plane,
            Transformed
              ( translate (D 0 300 100)
                  `composeTransform` rotate (N 1 0 0) ((1 / 1.1) * pi / 2)
              )
              $ object (Material white Mirror black) plane,
            Transformed
              ( translate (D 0 400 100)
                  `composeTransform` rotate (N 1 0 0) ((1 / 1.35) * pi / 2)
              )
              $ object (Material white Mirror black) plane,
              aLight (P 50 250 (-100)) 70 (C 1 0 0),
              aLight (P 150 250 (-100)) 50 (C 0 1 0),
              aLight (P 250 250 (-100)) 30 (C 0 0 1),
              aLight (P 350 250 (-100)) 10 (C 1 1 0)
          ]

    lights =
      [ Light (PointLight (P 0 450 (-100))) (C 0 10000 0)
      ]

    aLight p r emit = AttachMaterial (Material black Diffuse emit) (Spheres (buildBVH [Sphere p r]))

    black = C 0 0 0
    white = C 1 1 1

    object m s = AttachMaterial m (Triangles s)
