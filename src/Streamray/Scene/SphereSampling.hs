{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

module Streamray.Scene.SphereSampling where

import Streamray.Intersect
import Streamray.Light
import Streamray.Linear
import Streamray.Material
import Streamray.Ray
import Streamray.Scene

-- This is a scene with a huge sphere as background, in order to catch light,
-- and a sphere light which lit it.
-- This is intended to test the sampling quality of the sphere sampling.
sphereSampling :: Scene
sphereSampling = Scene objects lights
  where
    sphereLight = Sphere (P 250 250 250) 200
    emitSphereLight = C 50000 50000 50000
    objects =
      SceneBVH $
        buildBVH
          [ objectSingle
              (Material (C 0.6 0.6 0) Diffuse Nothing)
              (Sphere (P 250 250 (sphereRadius + 500)) sphereRadius)
          ]

    lights =
      [Light (SphereLight sphereLight) emitSphereLight]

    objectSingle m s = AttachMaterial m (Spheres $ buildBVH [s])

    -- Change the flatness of the walls
    sphereRadius = 5000
