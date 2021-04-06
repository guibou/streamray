{-# LANGUAGE DataKinds #-}

-- | Represents a static scene, a cornel box with one light
module Streamray.Scene where

import Streamray.Linear
import Streamray.Material
import Streamray.Ray

data Light = Light
  { position :: V3 'Position,
    emission :: V3 'Color
  }
  deriving (Show)

data Scene = Scene
  { objects :: [Object],
    lights :: [Light]
  }
  deriving (Show)

-- | A light
light :: Light
light = Light (P 250 250 250) (C 30000 30000 30000)

-- | Drives the flatness of the walls
sphereRadius :: Float
sphereRadius = 5000

-- | Initial scene. It is a box built with (big) spheres for the walls. Change
-- 'sphereRadius' in order to flatten the walls.
objects' :: [Object]
objects' =
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
      (Material (C 1 1 1) Mirror)
      (Sphere (P 150 350 250) 100),
    -- Small sphere 1
    Object
      (Material (C 1 1 1) (Glass 2.4))
      (Sphere (P 350 350 250) 100)
  ]

scene :: Scene
scene = Scene objects' [light]
