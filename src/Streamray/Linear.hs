{-# LANGUAGE DataKinds #-}
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
    V3 (..),
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

    -- * Patterns
    pattern P,
    pattern N,
    pattern D,
    pattern C,
    -- Unsafe API
    unsafeNormalized,
  )
where

import Data.Coerce

-- | Represents whether a direction is normalized or not
data DirectionKind = Normalized | NotNormalized

-- | Space for the points
data Space
  = -- | A position in 3D
    Position
  | -- | A direction in a 3D space. Can be normalized
    Direction DirectionKind
  | -- | A color measurement
    Color

-- | This is a point in 3D associated with a space
data V3 (k :: Space) = UnsafeV3 {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  deriving (Show, Eq)

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
applyFunctionOnPairOfV3Arguments f (UnsafeV3 x y z) (UnsafeV3 x' y' z') = UnsafeV3 (f x x') (f y y') (f z z')

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
  UnsafeV3 x y z .*. f = UnsafeV3 (x * f) (y * f) (z * f)

type family MulResultScalarV3 a where
  MulResultScalarV3 ('Direction k) = 'Direction 'NotNormalized
  MulResultScalarV3 'Color = 'Color

-- | Scale of a 'V3' with a scalar. It scales 'Color' and 'Direction'.
instance Mul Float (V3 k) where
  type MulResult Float (V3 k) = V3 (MulResultScalarV3 k)
  {-# INLINE (.*.) #-}
  f .*. UnsafeV3 x y z = UnsafeV3 (f * x) (f * y) (f * z)

-- * Ctor and aliases

-- | Position
pattern P :: Float -> Float -> Float -> V3 'Position
pattern P x y z = UnsafeV3 x y z

-- | Direction
pattern D :: Float -> Float -> Float -> V3 ('Direction 'NotNormalized)
pattern D x y z = UnsafeV3 x y z

-- | Normalized direction (this pattern will normalize the value for you)
pattern N :: Float -> Float -> Float -> V3 ('Direction 'Normalized)
pattern N x y z <-
  UnsafeV3 x y z
  where
    N x y z = UnsafeV3 x' y' z'
      where
        n = sqrt (x * x + y * y + z * z)
        x' = x / n
        y' = y / n
        z' = z / n

-- | Color
pattern C :: Float -> Float -> Float -> V3 'Color
pattern C x y z = UnsafeV3 x y z

{-# INLINE dot #-}

-- | Scalar product
dot :: V3 ('Direction k) -> V3 ('Direction k') -> Float
dot (UnsafeV3 x y z) (UnsafeV3 x' y' z') = x * x' + y * y' + z * z'

{-# INLINE normalize #-}

-- | Normalize a direction
normalize :: V3 ('Direction 'NotNormalized) -> V3 ('Direction 'Normalized)
normalize (UnsafeV3 x y z) = N x y z

{-# INLINE unsafeNormalized #-}

-- | Assume that the vector is normalized
unsafeNormalized :: V3 ('Direction 'NotNormalized) -> V3 ('Direction 'Normalized)
unsafeNormalized = coerce

{-# INLINE flipDirection #-}

-- | Flip a direction
flipDirection :: V3 ('Direction k) -> V3 ('Direction k)
flipDirection (UnsafeV3 x y z) = UnsafeV3 (- x) (- y) (- z)

{-# INLINE (-->) #-}

-- | Represents the direction between two points
(-->) :: V3 'Position -> V3 'Position -> V3 ('Direction 'NotNormalized)
x --> y = y .-. x

{-# COMPLETE P #-}
{-# COMPLETE N #-}
{-# COMPLETE D #-}
{-# COMPLETE C #-}
