{-# LANGUAGE DataKinds #-}

-- | Sampling interface
module Streamray.Sampling
  ( sampleCosinus,
    rotateVector,
    uniformF,
    uniform2F,
    makeBase,
    Sample2D (..),
    sampleCosinusMax,
    sampleSphere,
    sampleHemiSphere,
    sampleCosinusLobe,
    pdfSampleCosinusLobe,
    pdfSamplingCosinusMax,
    Base(..),
  )
where

import Streamray.Linear
import System.Random.Stateful

data Sample2D = Sample2D {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  deriving (Show)

-- | Sampling random point on unit sphere
-- From https://people.cs.kuleuven.be/~philip.dutre/GI/TotalCompendium.pdf
-- Formula 33
sampleSphere :: Sample2D -> (Float, V3 ('Direction 'Normalized))
sampleSphere (Sample2D u v) =
  let phi = 2 * pi * u
      sqrt_v_one_minus_v = sqrt (v * (1 - v))
   in ( 1 / (4 * pi),
        unsafeNormalized $
          D
            (2 * cos phi * sqrt_v_one_minus_v)
            (2 * sin phi * sqrt_v_one_minus_v)
            (1 - 2 * v)
      )

-- | Sampling random point on unit hemisphere
-- From https://people.cs.kuleuven.be/~philip.dutre/GI/TotalCompendium.pdf
-- Formula 34
sampleHemiSphere :: Sample2D -> (Float, V3 ('Direction 'Normalized))
sampleHemiSphere (Sample2D u v) =
  let phi = 2 * pi * u
      sqrt_one_minus_vv = sqrt (1 - v * v)
   in ( 1 / (2 * pi),
        unsafeNormalized $
          D
            (cos phi * sqrt_one_minus_vv)
            (sin phi * sqrt_one_minus_vv)
            v
      )

-- | Sampling proportional to cosinus weighted on hemisphere
-- From https://people.cs.kuleuven.be/~philip.dutre/GI/TotalCompendium.pdf
-- Formula 35: sampling proportional to cosinus weighted on hemisphere
sampleCosinus ::
  -- | Random sample
  Sample2D ->
  -- | (pdf, sampledDirection)
  (Float, V3 ('Direction 'Normalized))
sampleCosinus (Sample2D u v) =
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

-- | Sampling proportional to cosinus lobe around normal
-- From https://people.cs.kuleuven.be/~philip.dutre/GI/TotalCompendium.pdf
-- Formula 36
sampleCosinusLobe ::
  Float ->
  -- | Random sample
  Sample2D ->
  -- | (pdf, sampledDirection)
  (Float, V3 ('Direction 'Normalized))
sampleCosinusLobe n (Sample2D u v) =
  let phi = 2 * pi * u
      z = v ** (1 / (n + 1))
      sqrt_1_minus_z2 = sqrt $ 1 - v ** (2 / (n + 1))
   in ( (n + 1) / (2 * pi) * z ** n,
        unsafeNormalized $
          D
            (cos phi * sqrt_1_minus_z2)
            (sin phi * sqrt_1_minus_z2)
            z
      )
{-# INLINE sampleCosinusLobe #-}

{-# INLINE sampleCosinusMax #-}

pdfSampleCosinusLobe :: Float -> Float -> Float
pdfSampleCosinusLobe roughness cosTheta = (roughness + 1) / (2 * pi) * cosTheta ** roughness

--

-- | Sampling proportional to cosinus weighted on spherical cap
-- From https://people.cs.kuleuven.be/~philip.dutre/GI/TotalCompendium.pdf
-- Formula 35(extended): sampling proportional to cosinus weighted on hemisphere
sampleCosinusMax ::
  -- | Theta Max
  Float ->
  -- | Random sample
  Sample2D ->
  -- | (pdf, sampledDirection)
  (Float, V3 ('Direction 'Normalized))
sampleCosinusMax cos_theta_max (Sample2D u v) =
  let phi = 2 * pi * u
      sqrt_v = sqrt v
      -- theta = acos (sqrt v)
      theta = acos z
      z = sqrt (1 - v * sin_theta_max2)
      sin_theta_max2 = 1 - cos_theta_max * cos_theta_max
      sin_theta_max = sqrt sin_theta_max2
      sin_theta_max_sqrt_v = sin_theta_max * sqrt_v
   in ( cos theta / (pi * sin_theta_max2),
        unsafeNormalized $
          D
            (cos phi * sin_theta_max_sqrt_v)
            (sin phi * sin_theta_max_sqrt_v)
            z
      )

pdfSamplingCosinusMax :: Float -> Float -> Float
pdfSamplingCosinusMax cosThetaMax cosTheta = cosTheta / (pi * sin_theta_max2)
  where
    sin_theta_max2 = 1 - cosThetaMax * cosThetaMax

data Base = Base {-# UNPACK #-} !(V3 ('Direction 'Normalized)) {-# UNPACK #-} !(V3 ('Direction 'Normalized))
  deriving (Eq, Show)

{-# INLINE makeBase #-}

-- | Basis rotation, based on http://jcgt.org/published/0006/01/01/ Building an Orthonormal Basis, Revisited
makeBase ::
  -- | Normal (Z of the basis)
  V3 ('Direction 'Normalized) ->
  -- | (baseX, baseY)
  Base
makeBase (N x y z) = Base baseX baseY
  where
    sign = if z == 0 then 1 else signum z
    a = -1.0 / (sign + z)
    b = x * y * a

    baseX = unsafeNormalized $ D (1 + sign * x * x * a) (sign * b) (- sign * x)
    baseY = unsafeNormalized $ D b (sign + y * y * a) (- y)

-- | Rotate a vector around a normal
rotateVector ::
  -- | Normal
  V3 ('Direction 'Normalized) ->
  -- | Vector
  V3 ('Direction 'Normalized) ->
  V3 ('Direction 'Normalized)
rotateVector normal (N x y z) =
  let Base baseX baseY = makeBase normal
   in unsafeNormalized (x .*. baseX .+. y .*. baseY .+. z .*. normal)

-- | Uniform sampling of a random number in [0, 1]
uniformF :: StatefulGen g m => g -> m Float
uniformF = uniformRM (0, 1)

uniform2F :: StatefulGen g m => g -> m Sample2D
uniform2F g = Sample2D <$> uniformF g <*> uniformF g
