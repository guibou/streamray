{-# LANGUAGE DataKinds #-}

-- | Represents a static scene, a cornel box with one light
module Streamray.Scene where

import Streamray.Linear
import Streamray.Material
import Streamray.Ray
import Data.Maybe (mapMaybe)

data Light = Light
  { behavior :: LightBehavior,
    emission :: V3 'Color
  }
  deriving (Show)

data LightBehavior
  = PointLight (V3 'Position)
  | SphereLight Sphere
  deriving (Show)

-- TODO: it should be vector instead of list
data Scene = Scene
  { objects :: [Object],
    lights :: [Light]
  }
  deriving (Show)

-- | Drives the flatness of the walls
sphereRadius :: Float
sphereRadius = 5000

-- | Initial scene. It is a box built with (big) spheres for the walls. Change
-- 'sphereRadius' in order to flatten the walls.
objects' :: [Object]
objects' =
  [ Object
      (Material (C 0.6 0.6 0) Diffuse black)
      (Sphere (P (sphereRadius + 500) 250 0) sphereRadius), -- Right
    Object
      (Material (C 0 0.6 0.6) Diffuse black)
      (Sphere (P (- sphereRadius) 250 0) sphereRadius), -- Left
    Object
      (Material (C 0.6 0.6 0.6) Diffuse black)
      (Sphere (P 250 (- sphereRadius) 0) sphereRadius), -- Top
    Object
      (Material (C 0.6 0.6 0.6) Diffuse black)
      (Sphere (P 250 (sphereRadius + 500) 0) sphereRadius), -- Bottom
    Object
      (Material (C 0.6 0.6 0.6) Diffuse black)
      (Sphere (P 250 250 (sphereRadius + 500)) sphereRadius), -- Back
      -- Small sphere 1
    Object
      (Material (C 1 1 1) Mirror black)
      (Sphere (P 150 350 250) 100),
    -- Small sphere 1
    Object
      (Material (C 1 1 1) (Glass 2.4) black)
      (Sphere (P 350 350 250) 100),
    -- Small light 1
    Object
      (Material (C 0.5 0.5 0.5) Diffuse (C 10000 10000 10000))
      (Sphere (P 250 150 250) 30)
  ]

black :: V3 'Color
black = C 0 0 0

scene :: Scene
scene =
  Scene
    objects'
    (lights' ++ mapMaybe objectToLight objects')

lights' :: [Light]
lights' = [
    -- Light (PointLight (P 250 250 250)) (C 10000 10000 10000),
    Light (PointLight (P 50 250 250)) (C 30000 10000 10000),
    Light (PointLight (P 450 250 250)) (C 10000 10000 30000)
    ]

objectToLight :: Object -> Maybe Light
objectToLight (Object (Material _ _ emission) o)
  | emission == black = Nothing
  | otherwise = Just (Light (SphereLight o) emission)
