{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | This is the home for all the ray tracing function. It stores 'Ray', 'Object'
-- (which represents a 'Sphere' with a 'Material') as well as the different
-- raytracing functions.
module Streamray.Ray where

import Streamray.Linear
import Streamray.Material

-- | This is a ray
-- Any point X on the ray can be represented using X = Origin + t * Direction.
data Ray = Ray
  { origin :: V3 'Position,
    direction :: V3 ('Direction 'Normalized)
  }
  deriving (Show)

-- | This is a Sphere
data Sphere = Sphere
  { center :: V3 'Position,
    radius :: Float
  }
  deriving (Show)

-- | Represents a object, with its shape and material
data Object = Object Material Sphere
  deriving (Show)
