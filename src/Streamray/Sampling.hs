{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

-- | Sampling interface
module Streamray.Sampling
  ( sampleCosinus,
    rotateVector,
  )
where

import qualified Linear
import Streamray.Linear

-- | Sampling proportional to cosinus weighted on hemisphere
-- From https://people.cs.kuleuven.be/~philip.dutre/GI/TotalCompendium.pdf
-- Formula 35: sampling proportional to cosinus weighted on hemisphere
sampleCosinus ::
  -- | Random U
  Float ->
  -- | Random V
  Float ->
  -- | (pdf, sampledDirection)
  (Float, V3 ('Direction 'Normalized))
sampleCosinus u v =
  let phi = 2 * pi * u
      sqrt_v = sqrt v
      theta = acos (sqrt v)
      sqrt_1_minus_v = sqrt (1 - v)
   in ( cos theta / pi,
        unsafeNormalized $
          D
            (cos phi * sqrt_1_minus_v)
            (sin phi * sqrt_1_minus_v)
            sqrt_v
      )

-- | Basis rotation, based on http://jcgt.org/published/0006/01/01/ Building an Orthonormal Basis, Revisited
makeBase ::
  -- | Normal (Z of the basis)
  Linear.V3 Float ->
  -- | (baseX, baseY)
  (Linear.V3 Float, Linear.V3 Float)
makeBase (Linear.V3 x y z) = (baseX, baseY)
  where
    sign = signum z
    a = -1.0 / (sign + z)
    b = x * y * a

    baseX = Linear.V3 (1 + sign * x * x * a) (sign * b) (- sign * x)
    baseY = Linear.V3 b (sign + y * y * a) (- y)

-- | Rotate a vector around a normal
rotateVector :: V3 ('Direction 'Normalized) -- ^ Normal
  -> V3 ('Direction 'Normalized) -- ^ Vector
  -> V3 ('Direction 'Normalized)
rotateVector (unsafeToV3 -> normal) (unsafeToV3 -> Linear.V3 x y z) =
  let (baseX, baseY) = makeBase normal
   in unsafeV3 (x Linear.*^ baseX + y Linear.*^ baseY + z Linear.*^ normal)
