{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import qualified Control.Exception as E
import Control.Monad
import Data.Coerce
import Data.Maybe
import Streamray.Geometry.Box
import Streamray.Intersect
import Streamray.Linear
import Streamray.Material
import Streamray.Ray
import Streamray.Render
import Streamray.Sampling
import System.Random.Stateful
import qualified Test.HUnit.Lang
import Test.Hspec
import Test.QuickCheck hiding (scale)

-- | Newtype wrapper to sample "random" number in the 0-1 range.
newtype RandomFloat = RandomFloat Float
  deriving (Show)

instance Arbitrary RandomFloat where
  arbitrary = RandomFloat . abs . snd . (properFraction @Float @Int) <$> arbitrary

instance Arbitrary (V3 'Position) where
  arbitrary = P <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (V3 ('Direction 'Normalized)) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary

    flip <- arbitrary

    let z = (if flip then negate else id) $ sqrt (abs (1 - x * x - y * y))

    pure $ N x y z

instance Arbitrary (V3 ('Direction 'NotNormalized)) where
  arbitrary = (.*.) <$> arbitrary @Float <*> arbitrary @(V3 ('Direction 'Normalized))

instance Arbitrary (V3 'Color) where
  arbitrary = C <$> absA <*> absA <*> absA
    where
      absA = abs <$> arbitrary

instance Arbitrary Transform where
  arbitrary =
    oneof
      [ translate <$> arbitrary,
        -- Scaling does not negatively scale in order to avoid issues with winding order
        scale <$> arbitrary `suchThat` (\(D x y z) -> x * y * z /= 0) `suchThat` (\(D x y z) -> x >= 0 && y >= 0 && z >= 0),
        rotate <$> arbitrary <*> arbitrary
      ]

instance Arbitrary Sample2D where
  arbitrary = Sample2D <$> (snd . properFraction @_ @Int . abs <$> arbitrary) <*> (snd . properFraction @_ @Int . abs <$> arbitrary)

main :: IO ()
main = hspec $ do
  describe "box" $ do
    it "biggestAxis" $ do
      boxBiggestAxis (Box (P 0 0 0) (P 1 2 3)) `shouldBe` Z
      boxBiggestAxis (Box (P 0 0 0) (P 3 1 1)) `shouldBe` X
      boxBiggestAxis (Box (P 0 0 0) (P 1 3 2)) `shouldBe` Y
    it "union" $ do
      Box (P 0 0 0) (P 1 1 1) <> Box (P 2 3 1) (P 5 6 7) `shouldBe` Box (P 0 0 0) (P 5 6 7)
    it "fromSphere" $ do
      toBox (Sphere (P 1 2 3) 2) `shouldBe` Box (P (-1) 0 1) (P 3 4 5)
    it "property: mempty" $ do
      property $ \p0 p1 -> Box p0 p1 <> mempty `shouldBe` Box p0 p1
  describe "intersect" $ do
    describe "sphere" $ do
      it "in front" $ do
        rayIntersectSphere (Ray (P 0 0 0) (N 1 0 0)) (Sphere (P 10 0 0) 2) `shouldBe` Just 8
      it "from inside" $ do
        rayIntersectSphere (Ray (P 10 0 0) (N 1 0 0)) (Sphere (P 10 0 0) 2) `shouldBe` Just 2
      it "in back" $ do
        rayIntersectSphere (Ray (P 15 0 0) (N 1 0 0)) (Sphere (P 10 0 0) 2) `shouldBe` Nothing
      it "true miss" $ do
        rayIntersectSphere (Ray (P 0 10 0) (N 1 0 0)) (Sphere (P 10 0 0) 2) `shouldBe` Nothing
    describe "box" $ do
      let box = Box (P 3 0 0) (P 10 10 10)
      it "in front" $ do
        rayIntersectBox (Ray (P 0 1 1) (N 1 0 0)) box `shouldBe` Just 3
      it "from inside" $ do
        rayIntersectBox (Ray (P 8 1 1) (N 1 0 0)) box `shouldBe` Just 2
      it "in back" $ do
        rayIntersectBox (Ray (P 15 1 1) (N 1 0 0)) box `shouldBe` Nothing
      it "true miss" $ do
        rayIntersectBox (Ray (P 0 10 1) (N 1 0 0)) box `shouldBe` Nothing
    describe "box first offset" $ do
      let box = Box (P 3 0 0) (P 10 10 10)
      it "in front" $ do
        rayFirstOffsetInBox (Ray (P 0 1 1) (N 1 0 0)) box `shouldBe` Just 3
      it "from inside" $ do
        rayFirstOffsetInBox (Ray (P 8 1 1) (N 1 0 0)) box `shouldBe` Just 0
      it "in back" $ do
        rayFirstOffsetInBox (Ray (P 15 1 1) (N 1 0 0)) box `shouldBe` Nothing
      it "true miss" $ do
        rayFirstOffsetInBox (Ray (P 0 10 1) (N 1 0 0)) box `shouldBe` Nothing
  describe "material" $ do
    describe "reflect" $ do
      it "property: double reflect is id" $ do
        property $ \n wi -> reflect n (reflect n wi) `shouldApproxVector` wi

    describe "refcact" $ do
      it "property: does no change the vector if ior is 1" $
        property $ \n wi -> refract 1.0 n wi `shouldSatisfy` (approximateVector wi . snd . fromJust)
      it "total internal reflection" $ do
        -- Test with a strong IOR and a grazing angle
        refract 3 (N 0 0 1) (N 0.9 0.9 0.1) `shouldBe` Nothing
  describe "render" $ do
    it "property: sameside" $ do
      property $ \a b n -> sameSide n a b `shouldNotBe` sameSide n a (flipDirection b)

  describe "sampling" $ do
    describe "rotate vector" $ do
      it "property: do not rotate around Z" $ do
        property $ \wi -> rotateVector (N 0 0 1) wi `shouldApproxVector` wi
      it "property: rotating Z around anything" $ do
        property $ \n -> rotateVector n (N 0 0 1) `shouldApproxVector` n
    describe "make base" $ do
      it "works with 0 z" $ do
        makeBase (N 1 0 0) `shouldBe` (N 0 0 (-1), N 0 1 0)
      it "property: orthonormal" $ do
        property $ \n -> do
          let (baseX, baseY) = makeBase n
          baseX `dot` baseY `shouldApprox` 0
          baseX `dot` n `shouldApprox` 0
          n `dot` baseY `shouldApprox` 0
          norm (coerce baseX) `shouldApprox` 1
          norm (coerce baseY) `shouldApprox` 1
          norm (coerce n) `shouldApprox` 1

    describe "cosinus sampling" $ do
      it "property: always up the normal" $ do
        property $ \uv -> do
          let (_pdf, wo) = sampleCosinus uv
          wo `dot` N 0 0 1 `shouldSatisfy` (>= 0)
      -- Disabled right now. It fails with Sample2D 0 0
      xit "property: positive pdf" $ do
        property $ \uv -> do
          let (pdf, _wo) = sampleCosinus uv
          pdf `shouldSatisfy` (>= 0)
      -- WIP
      describe "integrates" $ do
        it "1" $ do
          -- See [Notes about integration]
          -- >>> integrate(sin(theta), (theta, 0, pi/2), (phi, 0, 2 * pi))
          -- 2⋅π
          runStateGen_ (mkStdGen 0) (integrate (const (1 :: Float)) sampleCosinus 10000) `shouldApprox` (2 * pi)
        it "cos" $ do
          -- >>> integrate(cos(theta) * sin(theta), (theta, 0, pi/2), (phi, 0, 2 * pi))
          -- π
          runStateGen_ (mkStdGen 0) (integrate (dot (N 0 0 1)) sampleCosinus 1000) `shouldApprox` pi

    describe "cosinus (clamped) sampling" $ do
      describe "integrates full hemisphere" $ do
        it "1" $ do
          runStateGen_ (mkStdGen 0) (integrate (const (1 :: Float)) (sampleCosinusMax 0) 10000) `shouldApprox` (2 * pi)
        it "cos" $ do
          runStateGen_ (mkStdGen 0) (integrate (dot (N 0 0 1)) (sampleCosinusMax 0) 10000) `shouldApprox` pi
      describe "property: variable a" $ do
        it "1" $ do
          -- >>> integrate(sin(theta), (theta, 0, cos(a)), (phi, 0, 2 * pi))
          -- 2⋅π⋅(1 - cos(cos(a)))
          property $ \(RandomFloat a) -> runStateGen_ (mkStdGen 0) (integrate (const (1 :: Float)) (sampleCosinusMax a) 10000) `shouldApprox` (2 * pi * (1 - a))

    describe "render" $ do
      describe "tonemap" $ do
        it "truncate negative" $ do
          truncateWord8 (-1) `shouldBe` 0
        it "truncate > 1" $ do
          truncateWord8 2 `shouldBe` 255

    describe "utils" $ do
      it "chunks of" $ do
        chunksOf 2 [1 .. 10] `shouldBe` [[1 :: Int, 2], [3, 4], [5, 6], [7, 8], [9, 10]]
  describe "transformation" $ do
    describe "identity" $ do
      it "identity translate does nothing" $ do
        property $ \p -> transformPoint (translate (D 0 0 0)) p `shouldBe` p
      it "identity rotate does nothing" $ do
        property $ \p n -> transformPoint (rotate n 0) p `shouldBe` p
      it "identity scale does nothing" $ do
        property $ \p -> transformPoint (scale (D 1 1 1)) p `shouldBe` p
    it "property: inverse t -> t is identity" $ do
      property $ \t p -> transformPoint t (transformPointInv t p) `shouldApproxVector` p
    it "property: inverse t -> t is identity" $ do
      property $ \t (p :: V3 ('Direction 'Normalized)) -> transformDirection t (transformDirectionInv t p) `shouldApproxVector` p
    it "Direction: translate invariant" $ do
      property $ \t d -> transformDirection (translate t) (d :: V3 ('Direction 'NotNormalized)) `shouldBe` d
    it "normal: translate invariant" $ do
      property $ \t d -> transformNormal (translate t) (d :: V3 ('Direction 'Normalized)) `shouldApproxVector` d
    describe "normal: scales correctly" $ do
      it "example" $ do
        -- x (1, 0, 0)
        -- y (0, 1, 0)
        -- n == N (1 1) --> N 1 0.5
        transformNormal (scale (D 1 2 1)) (N 1 1 0) `shouldApproxVector` N 1 0.5 0
      it "example with negative scale" $ do
        -- x (1, 0, 0) -> (1, 0, 0)
        -- y (0, 1, 0) -> (0, -2, 0)
        -- n == N (1 1) --> N 1 -0.5
        transformNormal (scale (D 1 (-2) 1)) (N 1 1 0) `shouldApproxVector` N 1 (-0.5) 0
      it "example with two negative scale" $ do
        -- x (1, 0, 0) -> (1, 0, 0)
        -- y (0, 1, 0) -> (0, -2, 0)
        -- n == N (1 1) --> N 1 -0.5
        transformNormal (scale (D (-1) (-2) 1)) (N 1 1 0) `shouldApproxVector` N (-1) (-0.5) 0
      it "property" $ do
        property $ \(v0 :: V3 ('Direction 'Normalized)) (v1 :: V3 ('Direction 'Normalized)) trans -> do
          abs (v0 `dot` v1) /= 1 ==> do
            let n = normalize $ v0 `cross` v1
            let v0' = transformDirection trans v0
            let v1' = transformDirection trans v1
            transformNormal trans n `shouldApproxVector` normalize (v0' `cross` v1')

shouldApprox :: (Ord a, Fractional a, Show a) => a -> a -> IO ()
shouldApprox a b = do
  unless (approx a b) $ do
    E.throwIO (Test.HUnit.Lang.HUnitFailure Nothing $ Test.HUnit.Lang.ExpectedButGot (Just "Both values are not close enough") (show a) (show b))

shouldApproxVector :: (Show a, Show a, Coercible a (V3 'Position), Coercible a (V3 'Position)) => a -> a -> IO ()
shouldApproxVector a b = do
  unless (approximateVector a b) $ do
    E.throwIO (Test.HUnit.Lang.HUnitFailure Nothing $ Test.HUnit.Lang.ExpectedButGot (Just "Both vector are not close enough") (show a) (show b))

approximateVector ::
  (Coercible a1 (V3 'Position), Coercible a2 (V3 'Position)) =>
  a1 ->
  a2 ->
  Bool
approximateVector a b = normSquared (coerce a --> coerce b) < 1e-5

approx :: (Ord a, Fractional a) => a -> a -> Bool
approx x a = abs (a - x) < 1e-1

integrate :: StatefulGen g m => (t -> Float) -> (Sample2D -> (Float, t)) -> Int -> g -> m Float
integrate f samplingFunction nSamples g = do
  res <- replicateM nSamples $ do
    (pdf, value) <- samplingFunction <$> uniform2F g
    pure $ f value / pdf

  pure $ sum res / fromIntegral nSamples

-- [Notes about integration]
-- sampleCosinus and sampleCosinusMax function are generating random samples on
-- a (hemi)sphere (ical cap). As such, they can be used to integrate function on a sphere.
--
-- Tests are done using the numerical 'integrate' function and are compared to
-- known analytic function computed in sympy. The computation formula is
-- written before each tests.
--
-- You will see that the computation formula includes a @sin(theta)@ term,
-- which represents the small surface element we use to integrate, which is
-- technically, sin(theta) dtheta dphi.
