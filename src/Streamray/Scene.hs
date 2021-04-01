{-# LANGUAGE DataKinds #-}
-- | Represents a static scene, a cornel box with one light
module Streamray.Scene where

import Streamray.Linear
import Streamray.Ray
import Streamray.Material

-- | A light
lightPosition :: V3 'Position
lightPosition = P 250 250 250

-- | Which emits
lightEmission :: V3 'Color
lightEmission = C 30000 30000 30000

-- | Drives the flatness of the walls
sphereRadius :: Float
sphereRadius = 5000

-- | Initial scene. It is a box built with (big) spheres for the walls. Change
-- 'sphereRadius' in order to flatten the walls. 
scene :: [Object]
scene =
  [ Object
      (Material (C 1 1 0) Diffuse)
      (Sphere (P (sphereRadius + 500) 250 0) sphereRadius), -- Right
    Object
      (Material (C 0 1 1) Diffuse)
      (Sphere (P (- sphereRadius) 250 0) sphereRadius), -- Left
    Object
      (Material (C 1 1 1) Diffuse)
      (Sphere (P 250 (- sphereRadius) 0) sphereRadius), -- Top
    Object
      (Material (C 1 1 1) Diffuse)
      (Sphere (P 250 (sphereRadius + 500) 0) sphereRadius), -- Bottom
    Object
      (Material (C 1 1 1) Diffuse)
      (Sphere (P 250 250 (sphereRadius + 500)) sphereRadius), -- Back
      -- Small sphere 1
    Object
      (Material (C 1 1 1) Diffuse)
      (Sphere (P 150 350 350) 80),
    -- Small sphere 1
    Object
      (Material (C 1 1 1) Diffuse)
      (Sphere (P 350 350 350) 80)
  ]


