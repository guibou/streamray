{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--   Linear algebra for computer graphics.
--
--   This module wraps 'Linear.V3' and introduces a few newtypes, based around
--   phantom types and the 'V3' type, in order to represents 'Position',
--   'Direction Normalized' (i.e. normal), 'Direction NotNormalized' and 'Color.
--
--   The patterns 'P', 'N', 'D', and 'C' are safe way in order to build such
--   types.
--
--   Combinations functions, such as '(.-.)', '(.+.)', '(.*.)' only allows
--   operations which are meaningful in this context. For example, adding a
--   'Position' and a 'Color' is not allowed, but subtracting 'Position' results
--   in not normalized 'Direction'.
--
--   In also reexport functions from Linear with the safe types.
module Streamray.Linear
  ( -- * Types
    V3,
    Space (..),
    DirectionKind (..),

    -- * Classes
    Add (..),
    Mul (..),
    Sub (..),

    -- * Directions
    normalize,
    dot,
    flipDirection,
    (-->),
    norm,
    normSquared,
    cross,

    -- * Patterns
    pattern P,
    pattern N,
    pattern D,
    pattern C,
    -- Unsafe API
    unsafeNormalized,

    -- * Transformations
    Transform,
    translate,
    scale,
    rotate,
    rotateX,
    rotateY,
    rotateZ,
    inverseTransform,
    composeTransform,
    transformPoint,
    transformDirection,
    transformNormal,
  )
where

import Control.DeepSeq
import Control.Exception (assert)
import Data.Coerce
import GHC.Generics
import qualified Linear

-- | Represents whether a direction is normalized or not
data DirectionKind = Normalized | NotNormalized

-- | Space for the points
data Space
  = -- | A position in 3D
    Position
  | -- | A direction in a 3D space. Can be normalized
    Direction DirectionKind
  | -- | A color measurement
    -- TODO: should I introduce a "normalized" and non normalized color?
    Color

-- | This is a point in 3D associated with a space
data V3 (k :: Space) = V3 {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  deriving (Show, Eq, NFData, Generic)

-- * Add

-- | Add two items together
class Add a b where
  type AddResult a b
  (.+.) :: a -> b -> AddResult a b

infixl 6 .+.

type family AddResultV3 (a :: Space) (b :: Space) where
-- Color addition
  AddResultV3 'Color 'Color = 'Color
-- Translation
  AddResultV3 'Position ('Direction k) = 'Position
-- Translation
  AddResultV3 ('Direction k) 'Position = 'Position
-- Composition of direction
  AddResultV3 ('Direction k) ('Direction k') = 'Direction 'NotNormalized

{-# INLINE applyFunctionOnPairOfV3Arguments #-}

-- | This is used to apply a function inside the V3 newtype
applyFunctionOnPairOfV3Arguments :: (Float -> Float -> Float) -> V3 k1 -> V3 k2 -> V3 k3
applyFunctionOnPairOfV3Arguments f (V3 x y z) (V3 x' y' z') = V3 (f x x') (f y y') (f z z')

-- | Instance for V3. It represents 'Color' addition, 'Position' translations
-- and 'Direction' compositions.
instance Add (V3 a) (V3 b) where
  type AddResult (V3 a) (V3 b) = V3 (AddResultV3 a b)
  {-# INLINE (.+.) #-}
  (.+.) = applyFunctionOnPairOfV3Arguments (+)

-- | Subtract two items.
class Sub a b where
  type SubResult a b
  (.-.) :: a -> b -> SubResult a b

infixl 6 .-.

type family SubResultV3 a b where
  SubResultV3 'Position ('Direction k) = 'Position
  SubResultV3 'Position 'Position = 'Direction 'NotNormalized
  SubResultV3 ('Direction k) ('Direction k') = 'Direction 'NotNormalized

-- | Instance for V3. It represents 'Position' differences (aka 'Direction'),
-- translation and 'Direction' compositions.
instance Sub (V3 a) (V3 b) where
  type SubResult (V3 a) (V3 b) = V3 (SubResultV3 a b)
  {-# INLINE (.-.) #-}
  (.-.) = applyFunctionOnPairOfV3Arguments (-)

-- | Multiply two types
class Mul a b where
  type MulResult a b
  (.*.) :: a -> b -> MulResult a b

infixl 7 .*.

type family MulResultV3 a b where
  MulResultV3 'Color 'Color = 'Color

-- | Instance for V3. It represents 'Color' mix.
instance Mul (V3 a) (V3 b) where
  type MulResult (V3 a) (V3 b) = V3 (MulResultV3 a b)
  {-# INLINE (.*.) #-}
  (.*.) = applyFunctionOnPairOfV3Arguments (*)

-- | Scale of a 'V3' with a scalar. It scales 'Color'.
-- and 'Direction'.
instance Mul (V3 k) Float where
  type MulResult (V3 k) Float = V3 (MulResultScalarV3 k)
  {-# INLINE (.*.) #-}
  V3 x y z .*. f = V3 (x * f) (y * f) (z * f)

type family MulResultScalarV3 a where
  MulResultScalarV3 ('Direction k) = 'Direction 'NotNormalized
  MulResultScalarV3 'Color = 'Color

-- | Scale of a 'V3' with a scalar. It scales 'Color' and 'Direction'.
instance Mul Float (V3 k) where
  type MulResult Float (V3 k) = V3 (MulResultScalarV3 k)
  {-# INLINE (.*.) #-}
  f .*. V3 x y z = V3 (f * x) (f * y) (f * z)

-- * Ctor and aliases

-- | Position
pattern P :: Float -> Float -> Float -> V3 'Position
pattern P x y z = V3 x y z

-- | Direction
pattern D :: Float -> Float -> Float -> V3 ('Direction 'NotNormalized)
pattern D x y z = V3 x y z

-- | Normalized direction (this pattern will normalize the value for you)
pattern N :: Float -> Float -> Float -> V3 ('Direction 'Normalized)
pattern N x y z <-
  V3 x y z
  where
    N x y z = normalize (D x y z)

-- | Color
pattern C :: Float -> Float -> Float -> V3 'Color
pattern C x y z = V3 x y z

{-# INLINE dot #-}

-- | Scalar product
dot :: V3 ('Direction k) -> V3 ('Direction k') -> Float
dot (V3 x y z) (V3 x' y' z') = x * x' + y * y' + z * z'

{-# INLINE normalize #-}

-- | Normalize a direction
normalize :: V3 ('Direction 'NotNormalized) -> V3 ('Direction 'Normalized)
normalize (V3 x y z) = V3 x' y' z'
  where
    n = norm (D x y z)
    x' = x / n
    y' = y / n
    z' = z / n

{-# INLINE norm #-}

-- | Return the norm
norm :: V3 ('Direction 'NotNormalized) -> Float
norm = sqrt . normSquared

{-# INLINE normSquared #-}

-- | Return the squared norm
normSquared :: V3 ('Direction 'NotNormalized) -> Float
normSquared (V3 x y z) = x * x + y * y + z * z

{-# INLINE unsafeNormalized #-}

-- | Assume that the vector is normalized
unsafeNormalized :: V3 ('Direction 'NotNormalized) -> V3 ('Direction 'Normalized)
-- The assertion here checks that the vector is indeed correctly normalized
unsafeNormalized v = assert (normSquared v - 1 < 0.0001) $ coerce v

{-# INLINE flipDirection #-}

-- | Flip a direction
flipDirection :: V3 ('Direction k) -> V3 ('Direction k)
flipDirection (V3 x y z) = V3 (- x) (- y) (- z)

{-# INLINE (-->) #-}

-- | Represents the direction between two points
(-->) :: V3 'Position -> V3 'Position -> V3 ('Direction 'NotNormalized)
x --> y = y .-. x

{-# COMPLETE P #-}

{-# COMPLETE N #-}

{-# COMPLETE D #-}

{-# COMPLETE C #-}

-- | Cross product for any vector
cross ::
  V3 ('Direction t) ->
  V3 ('Direction k) ->
  V3 ('Direction 'NotNormalized)
cross (V3 x y z) (V3 x' y' z') = V3 (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

-- * Matrices transformations

-- | Opaque type which represents a transformation
newtype Transform = Transform (Linear.M44 Float)
  deriving (Show, Generic, NFData)

-- | Invert a transformation
inverseTransform :: Transform -> Transform
inverseTransform (Transform m) = Transform (Linear.inv44 m)

-- | This is a translation
translate :: V3 ('Direction 'NotNormalized) -> Transform
translate (D dx dy dz) = Transform $ Linear.V4 (Linear.V4 1 0 0 dx) (Linear.V4 0 1 0 dy) (Linear.V4 0 0 1 dz) (Linear.V4 0 0 0 1)

-- | This is a scaling matrix.
scale :: V3 ('Direction 'NotNormalized) -> Transform
scale (D sx sy sz) =
  Transform $
    Linear.V4
      (Linear.V4 sx 0 0 0)
      (Linear.V4 0 sy 0 0)
      (Linear.V4 0 0 sz 0)
      (Linear.V4 0 0 0 1)

rotateX, rotateY, rotateZ :: Float -> Transform

-- | Rotate around the Z axis by an angle in radians
rotateZ = rotate (N 0 0 1)

-- | Rotate around the Y axis by an angle in radians
rotateY = rotate (N 0 1 0)

-- | Rotate around the X axis by an angle in radians.
rotateX = rotate (N 1 0 0)

-- | Generalized rotation of an angle in radians around an arbitrary vector.
rotate :: V3 ('Direction 'Normalized) -> Float -> Transform
-- This implementation uses the internal quaternions from the linear package, because that's easier.
-- However, during the stream, we detailled rotation around Z axis using the following matrice:
--
-- cos(a), -sin(a), 0, 0
-- sin(a),  cos(a), 0, 0
-- 0     ,       0, 1, 0
-- 0     ,       0, 0, 1
rotate (V3 x y z) angle = Transform (Linear.mkTransformation (Linear.axisAngle (Linear.V3 x y z) angle) (Linear.V3 0 0 0))

-- | Transform a point
transformPoint :: Transform -> V3 'Position -> V3 'Position
transformPoint (Transform m) (V3 x y z) =
  -- We just use matrix multiplication with an homogeneous vector with w = 1
  let Linear.V4 x' y' z' w = m Linear.!* Linear.V4 x y z 1
   in assert (w == 1) $ V3 x' y' z'

-- | Transform a normal
-- This is a bit more convoluted.
-- Consider it like that. We have two points, A and B. And a normal to this
-- vector, N. Such that:
--
-- (AB) `dot` N = 0
--
-- We can write that in matrix form, introducing (Mt . Mt-1) which is thhe identity in th emiddle.
--
-- (note: Mt is the transpose of M)
--
-- AB . Mt . Mt-1 . N = 0
--
-- Now, once the transformation M is done, this equality should remain.
--
-- ==>
--  (M . ABt)t . (Nt . Mt-1t)t
--
--  (M.ABt)t is equal to AB' (the tnsformed version of AB using the M matrice)
--
--  So, in order for the equality to stay true, (Nt . Mt-1t)t is the new normal. Let's develop that.
--
--  (Nt . Mt-1t)t
--  Mt-1tt . N
--  Mt-1 . N
--
--  So the M' transformation which should be applied to the normal is Mt-1
--
--
--  Another way of developing that is as following.
--
--  We observed that the transformation we would like to apply is the same, with the scale inversed. Said otherwise, if
--
--  M = S * R
--
--  (still ignoring the T component)
--
--  Then
--
--  M' = S-1 * R
--
--  (With the Scale componenent inversed).
--
--  For example, if you have a line between (1,0) and (0, 1), the normal is
--  (once normalized) (1, 1). If we scale that twice over the X axis, we get a
--  line between (2, 0) and (0, 1). The normal (once normalized) is (0.5, 1). (Draw it).
--
--  Let's develop a bit
--
--  M' = S-1 * R
--  M  = (S-1 * R)-1-1
--
--  (double inverse does not change)
--
--  = (R-1 * S-1-1)-1
--  = (R-1 * S)-1
--  (Inverse distribute and inverse the composition)
--
--  = (R-1tt * Stt)-1
--
--  (Added double transpose, which does not change anything)
--
--  The transpose of the Scale matrix, is equal to itself. The transpose of the
--  rotation matrix is equal to its inverse.
--
--  ==>
--
--   = (Rt * St)-1
--
--  (Distribute the t similarly to the inverse)
--
--   = (S * R)t-1
--   = Mt-1
--
--   Which is equal to M'
transformNormal :: Transform -> V3 ('Direction 'Normalized) -> V3 ('Direction 'Normalized)
transformNormal (Transform m) (V3 x y z) =
  let Linear.V4 x' y' z' w = Linear.inv44 (Linear.transpose m) Linear.!* Linear.V4 x y z 0
   in assert (w == 0) $ N x' y' z'

-- | Transform a direction of any kind
transformDirection :: Transform -> V3 ('Direction k) -> V3 ('Direction k)
transformDirection (Transform m) (V3 x y z) =
  -- We use matrix multiplication with an homogeneous vector with w = 0
  let Linear.V4 x' y' z' w = m Linear.!* Linear.V4 x y z 0
   in assert (w == 0) $ V3 x' y' z'

-- Compose two transformations
--
-- @composeTransform a b@ returns a transformation which first apply
-- transformation @a@ then @b@.
composeTransform :: Transform -> Transform -> Transform
composeTransform (Transform a) (Transform b) = Transform (a Linear.!*! b)
