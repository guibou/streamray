{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

-- | This is the core of the rendering algorithm, with the main raytrace
-- "integrator", called 'radiance', as well as a naive camera model and image
-- saving.
module Streamray.Render where

import Codec.Picture
import Codec.Picture.Types (newMutableImage, unsafeFreezeImage)
import Control.Concurrent.Async
import Control.Monad (replicateM)
import Data.Foldable
import Data.Maybe
import Streamray.Intersect
import Streamray.Light
import Streamray.Linear
import Streamray.Material
import Streamray.Ray
import Streamray.RenderSettings
import Streamray.Sampling
import Streamray.Scene
import System.Random.Stateful
import Control.Exception (evaluate)
import Control.DeepSeq
import Data.Time (getCurrentTime, diffUTCTime)
import PyF
import Data.Time.Clock (NominalDiffTime)
import GHC.Stats

-- | Compute the direct lighting for a diffuse material
directLighting ::
  StatefulGen g m =>
  Scene ->
  -- | Position of the lighting
  V3 'Position ->
  -- | Surface normal (object must be on the positive side of the normal)
  V3 ('Direction k) ->
  -- | Light
  Light ->
  g ->
  m (V3 'Color)
directLighting scene x normal light g = do
  (directionToLight, coefNormLight) <- case Streamray.Light.behavior light of
    PointLight p -> pure (x --> p, 1)
    SphereLight Sphere {radius, center} -> do
      -- Indirect lighting
      uv <- uniform2F g

      let sphereRotationAxis = normalize (center --> x)

      -- Sample a direction proportional to cosinus
      let (pdf, sampledDirection) = rotateVector sphereRotationAxis <$> sampleCosinus uv
      let pointOnLight = center .+. radius .*. sampledDirection
      let directionToLight = x --> pointOnLight
      let cosFactor = - dot (normalize (center --> pointOnLight)) (normalize directionToLight)

      pure (x --> pointOnLight, (radius * radius * cosFactor / pi) / (pdf * radius * radius))
  let directionToLightNormalized = normalize directionToLight

      -- Diffuse shading
      coef = max 0 (dot normal directionToLightNormalized / (pi * lightDistance2))

      lightDistance2 = dot directionToLight directionToLight

      -- Trace a ray toward the light source and check for intersection
      canSeeLightSource =
        testRayVisibilityBVH
          ( Ray
              -- The origin of the ray is biased toward the light to avoid self
              -- shadows if the point is slightly under the surface due to
              -- floating point approximations.
              ( x
                  .+. epsilon .*. directionToLightNormalized
              )
              directionToLightNormalized
          )
          (objects scene)
          -- TODO: optimise this
          (lightDistance2 + 4 * epsilon * epsilon - 4 * epsilon * sqrt lightDistance2)

      visibility = if canSeeLightSource then C 1 1 1 else C 0 0 0

  pure $ coefNormLight .*. visibility .*. Streamray.Light.emission light .*. coef

-- | Returns the pixel color associated with a 'Ray'. It performs russian rulette.
radiance :: StatefulGen g m => Scene -> Bool -> Int -> Ray -> g -> m (Maybe (V3 'Color))
radiance _ _ 5 _ _ = pure $ Just (C 0 0 0)
radiance scene lastWasSpecular depth ray g = do
  r <- uniformF g

  let coefRR = if depth == 0 then 1 else 0.5

  if r < coefRR
    then fmap (((1 :: Float) / coefRR) .*.) <$> subRadiance scene lastWasSpecular depth ray g
    else pure $ Just (C 0 0 0)

-- | Returns the pixel color associated with a 'Ray'. This is the same as
-- 'radiance', but it does not perform russian rulette.
subRadiance :: forall g m. StatefulGen g m => Scene -> Bool -> Int -> Ray -> g -> m (Maybe (V3 'Color))
subRadiance scene lastWasSpecular depth ray g = case rayIntersectBVH ray (objects scene) of
  Nothing -> pure Nothing
  Just (Intersection (Object (Material albedo behavior emission) sphere) t) -> do
    let x = origin ray .+. t .*. direction ray
        normal = normalize (center sphere --> x)

        -- Mirror contribution may be used in multiple material path (Mirror
        -- and Glass), so we factorize the code here.
        mirrorContribution = do
          let reflectedDirection = reflect normal (direction ray)
              reflectedRay = Ray (x .+. epsilon .*. reflectedDirection) reflectedDirection
          contrib <- radiance scene True (depth + 1) reflectedRay g
          pure $ fromMaybe (C 0 0 0) contrib

    Just . (albedo .*.) <$> case behavior of
      Glass ior -> do
        -- flip the normal so that it is pointing outside.
        let transmittedDirectionMaybe = refract ior normal (direction ray)
        case transmittedDirectionMaybe of
          -- Total internal reflection, just mirror.
          Nothing -> mirrorContribution
          -- Glass contribution, with refraction and transmission
          Just (coef, transmittedDirection) -> do
            (r :: Float) <- uniformF g

            -- Pick a random choice, with probability (PDF) = coef
            if r < coef
              then do
                -- This choice was picked with PDF = coef, and is weighted by coef (the transmission factor). Both cancels
                let transmittedRay = Ray (x .+. (epsilon * 3) .*. transmittedDirection) transmittedDirection
                fromMaybe (C 0 0 0) <$> radiance scene True (depth + 1) transmittedRay g
              else -- This choice was picked with PDF = 1 - coef, and is weighted by 1 - coef (the transmission factor). Both cancels
                mirrorContribution
      Mirror -> mirrorContribution
      Diffuse -> do
        -- Sample uniformly one light
        -- TODO: we would like to do that by importance
        lu <- uniformRM (0, length (lights scene) - 1) g
        directLightContrib' <- directLighting scene x normal (lights scene !! lu) g
        let directLightContrib = (fromIntegral (length (lights scene)) :: Float) .*. directLightContrib'

        -- Indirect lighting
        uv <- uniform2F g
        -- Sample a direction proportional to cosinus
        let (_pdf, indirectDirection) = rotateVector normal <$> sampleCosinus uv

            -- The surface value is cos / pi, which is equal to the pdf. So they cancels.
            -- coefIndirect = (dot indirectDirection normal / pi) / pdf
            coefIndirect :: Float = 1
            indirectRay = Ray (x .+. epsilon .*. indirectDirection) indirectDirection

        contribIndirect <- fromMaybe (C 0 0 0) <$> radiance scene False (depth + 1) indirectRay g

        pure $ directLightContrib .+. (coefIndirect .*. contribIndirect) .+. (if lastWasSpecular then emission else C 0 0 0)

-- | @sameSide n a b@ returns 'True' if @a@ and @b@ are on the same side of the
-- normal @n@.
sameSide :: V3 ('Direction 'Normalized) -> V3 ('Direction 'Normalized) -> V3 ('Direction 'Normalized) -> Bool
sameSide n v0 v1 = (dot n v0 * dot n v1) > 0

-- | Changes the ray offset used to escape surface
epsilon :: Float
epsilon = 0.01

-- | Apply a gamma correction
gammaCorrect :: Float -> Float
gammaCorrect = (** (1 / 2.2))

-- | Convert a light measure to a pixel value
tonemap :: Float -> V3 'Color -> PixelRGBA8
tonemap alpha v = PixelRGBA8 x y z (truncateWord8 alpha)
  where
    -- truncate converts to Word8
    -- min/max clamps to the acceptable range
    -- pow (1 / 2.2) is doing gamma correction
    C (ftonemap -> x) (ftonemap -> y) (ftonemap -> z) = v

-- | Clamp a float from [0..1] to Word8 [0..255]
truncateWord8 :: Float -> Pixel8
truncateWord8 = truncate @Float @Pixel8 . max 0 . min 255 . (* 255)

-- | Converts a value from a measure to the RGB colorspace in 8bit.
ftonemap :: Float -> Pixel8
-- truncate converts to Word8
-- min/max clamps to the acceptable range
-- pow (1 / 2.2) is doing gamma correction
ftonemap = truncateWord8 . gammaCorrect

-- | Raytrace a 500x500 image
-- This function is called for each pixel
raytrace :: forall g m. StatefulGen g m => Int -> Scene -> Int -> Int -> g -> m PixelRGBA8
raytrace nSamples scene (fromIntegral -> x) (fromIntegral -> y) g = do
  -- Oversample the pixel
  rs <- replicateM nSamples runRay

  -- TODO: restore alpha
  let contribs = catMaybes rs
  let nAlphaContribs = nSamples - length contribs
  let contrib = foldl' (.+.) (C 0 0 0) contribs

  pure $ tonemap (1.0 - fromIntegral nAlphaContribs / fromIntegral nSamples) ((1 / fromIntegral nSamples :: Float) .*. contrib)
  where
    runRay = do
      -- Box filtering of 1x1
      -- TODO: we should move to FIS filtering for smoother results
      dx <- uniformF g
      dy <- uniformF g
      let -- Generate a ray in the XY plane and pointing in the Z direction
          coefOpening = 1.001

          -- TODO. Discuss about proper filtering
          n = P (x + dx) (y + dy) 0

          -- n' is on the plane [-250:250]
          n'@(P x' y' _) = n .-. D 250 250 0
          f = P (coefOpening * x') (coefOpening * y') 1

          d = normalize (n' --> f)
          ray = Ray n d
      radiance scene True 0 ray g

-- | Raytrace a 500x500 image, using the default scene, and saves it.
raytraceImage :: RenderSettings -> IO ()
raytraceImage renderSettings = do
    t <- getCurrentTime
    scene' <- evaluate $ force scene
    t' <- getCurrentTime
    let bvhTime = t' `diffUTCTime` t
    putStrLn [fmt|BVH time: {bvhTime:s}|]
    writePng (filepath renderSettings) =<< withImageParallel 500 500 (\x y -> runStateGen_ (mkStdGen ((x + 1) * (y + 1))) (raytrace (samplesPerPixel renderSettings) scene' x y))
    t'' <- getCurrentTime
    let totalTime = t'' `diffUTCTime` t
    let renderTime = t'' `diffUTCTime` t'
    putStrLn [fmt|Rendering time: {renderTime:s} {realToFrac @NominalDiffTime @Double $ renderTime / totalTime:.0%}|]
    putStrLn [fmt|Total time: {t'' `diffUTCTime` t:s}|]
    rtsStats <- getRTSStats
    putStrLn [fmt|Max memory usage: {max_mem_in_use_bytes rtsStats `div` 1000 `div` 1000: d} MiB|]
  where
    scene = loadKnownScene renderSettings

-- | Render an image in parallel
withImageParallel ::
  -- | Width
  Int ->
  -- | Height
  Int ->
  -- | @f x y@ returns the value for this pixel.
  (Int -> Int -> PixelRGBA8) ->
  IO (Image PixelRGBA8)
withImageParallel w h f = do
  im <- newMutableImage w h

  forConcurrently_ (chunksOf 10 [0 .. (h -1)]) $ \ys -> do
    for_ ys $ \y -> do
      for_ [0 .. (w -1)] $ \x -> writePixel im x y (f x y)

  unsafeFreezeImage im

-- | Splits a list in a few chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)
