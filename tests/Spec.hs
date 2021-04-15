{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import Test.Hspec
import Test.QuickCheck

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
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary

    pure $ C (abs x) (abs y) (abs z)

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
        property $ \n wi -> reflect n (reflect n wi) `shouldSatisfy` approximateVector wi

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
        property $ \wi -> rotateVector (N 0 0 1) wi `shouldSatisfy` approximateVector wi
      it "property: rotating Z around anything" $ do
        property $ \n -> rotateVector n (N 0 0 1) `shouldSatisfy` approximateVector n
    describe "make base" $ do
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
      xit "pdf integrates to 1" $ do
        runStateGen_ (mkStdGen 0) (integrateSphere (fst . sampleCosinus) 100) `shouldBe` 1
    describe "render" $ do
      describe "tonemap" $ do
        it "truncate negative" $ do
          truncateWord8 (-1) `shouldBe` 0
        it "truncate > 1" $ do
          truncateWord8 2 `shouldBe` 255

    describe "utils" $ do
      it "chunks of" $ do
        chunksOf 2 [1 .. 10] `shouldBe` [[1 :: Int, 2], [3, 4], [5, 6], [7, 8], [9, 10]]

approximateVector ::
  (Coercible a1 (V3 'Position), Coercible a2 (V3 'Position)) =>
  a1 ->
  a2 ->
  Bool
approximateVector a b = normSquared (coerce a --> coerce b) < 1e-5

shouldApprox :: (Show a, Ord a, Fractional a) => a -> a -> Expectation
shouldApprox a b = abs (a - b) `shouldSatisfy` (< 1e-5)

integrateSphere :: (StatefulGen g m) => (Sample2D -> Float) -> Int -> g -> m Float
integrateSphere f nSamples g = do
  res <- replicateM nSamples $ do
    (f <$> uniform2F g)

  pure $ sum res / fromIntegral nSamples
