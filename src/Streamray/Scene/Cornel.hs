{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

module Streamray.Scene.Cornel where

import Streamray.Intersect
import Streamray.Light
import Streamray.Linear
import Streamray.Material
import Streamray.Ray
import Streamray.Scene

-- | Initial scene. It is a box built with (big) spheres for the walls. Change
-- 'sphereRadius' in order to flatten the walls.
cornel :: Scene
cornel = Scene objects lights
  where
    sphereLight = Sphere (P 250 150 250) 30
    emitSphereLight = Just $ Light (SphereLight sphereLight) (C 10000 10000 10000)
    objects =
      SceneBVH $
        buildBVH
          [ objectSingle
              (Material (C 0.6 0.6 0) Diffuse black)
              (Sphere (P (sphereRadius + 500) 250 0) sphereRadius), -- Right
            objectSingle
              (Material (C 0 0.6 0.6) Diffuse black)
              (Sphere (P (- sphereRadius) 250 0) sphereRadius), -- Left
            objectSingle
              (Material (C 0.6 0.6 0.6) Diffuse black)
              (Sphere (P 250 (- sphereRadius) 0) sphereRadius), -- Top
            objectSingle
              (Material (C 0.6 0.6 0.6) Diffuse black)
              (Sphere (P 250 (sphereRadius + 500) 0) sphereRadius), -- Bottom
            objectSingle
              (Material (C 0.6 0.6 0.6) Diffuse black)
              (Sphere (P 250 250 (sphereRadius + 500)) sphereRadius), -- Back
              -- Small sphere 1
            objectSingle
              (Material (C 1 1 1) Mirror black)
              (Sphere (P 150 350 250) 100),
            -- Small sphere 1
            objectSingle
              (Material (C 1 1 1) (Glass 2.4) black)
              (Sphere (P 350 350 250) 100),
            -- Small light 1
            objectSingle
              (Material (C 0.5 0.5 0.5) Diffuse emitSphereLight)
              sphereLight
          ]

    lights =
      [ -- Light (PointLight (P 250 250 250)) (C 10000 10000 10000),
        Light (PointLight (P 50 250 250)) (C 30000 10000 10000),
        Light (PointLight (P 450 250 250)) (C 10000 10000 30000),
        -- This is the sphere light with emission
        -- TODO: copy it from the objects list
        Light (SphereLight sphereLight) (C 10000 10000 10000)
      ]
    black = Nothing

    objectSingle m s = AttachMaterial m (Spheres $ buildBVH [s])

    -- Change the flatness of the walls
    sphereRadius = 5000
