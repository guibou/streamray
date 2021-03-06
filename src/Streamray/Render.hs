{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | This is the core of the rendering algorithm, with the main raytrace
-- "integrator", called 'radiance', as well as a naive camera model and image
-- saving.
module Streamray.Render where

import Codec.Picture
import Codec.Picture.Types (newMutableImage, unsafeFreezeImage)
import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad (replicateM, when)
import Data.Foldable
import Data.Maybe
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Time.Clock (NominalDiffTime)
import Debug.Trace
import GHC.IO.Unsafe
import GHC.Stats
import PyF
import Streamray.Intersect
import Streamray.Light
import Streamray.Linear
import Streamray.Material
import Streamray.Ray
import Streamray.RenderSettings
import Streamray.Sampling
import Streamray.Scene
import System.Random.Stateful

-- | Compute the direct lighting for a diffuse material
directLighting ::
  StatefulGen g m =>
  Scene ->
  -- | Incoming ray direction
  V3 ('Direction 'Normalized) ->
  -- | Position of the lighting
  V3 'Position ->
  -- | Surface normal (object must be on the positive side of the normal)
  V3 ('Direction 'Normalized) ->
  -- | Light
  Light ->
  -- | Roughness
  Float ->
  g ->
  m (V3 'Color)
directLighting scene wi x normal light roughness g = do
  (directionToLight, coefNormLight) <- case Streamray.Light.behavior light of
    PointLight p -> pure (x --> p, 1)
    SphereLight Sphere {radius, center} -> do
      -- Indirect lighting
      uv <- uniform2F g

      let sphereRotationAxis = normalize (center --> x)

      -- Sample a direction proportional to cosinus in the visible spherical cap
      --
      --                          O--- - - - - - - - - - -\
      --                         /                       \
      --                        /                        \
      --  x ------------------- P----------- c
      --
      -- (use your imagination, the point x is lit by the sphere light centered on c)
      --  There is a point on the spherea, at O were the sphere is no longer able to see the point x.
      --  We compute the cosinus of the angle PcO:
      let cos_theta_max = radius / norm (center --> x)
      -- And the we sample points only on the spherical cap (drawed here with /)
      --
      -- TODO; solve NaN issue when x is too close to the sphere
      let (pdf', sampledDirection) = rotateVector sphereRotationAxis <$> sampleCosinusMax (min 0.9 cos_theta_max) uv
      -- let (pdf, sampledDirection) = rotateVector sphereRotationAxis <$> sampleSphere uv
      --let (pdf, sampledDirection) = rotateVector sphereRotationAxis <$> sampleHemiSphere uv
      -- let (pdf, sampledDirection) = rotateVector sphereRotationAxis <$> sampleCosinus uv
      let pointOnLight = center .+. radius .*. sampledDirection
      let directionToLight = x --> pointOnLight
      let cosFactor = - dot (normalize (center --> pointOnLight)) (normalize directionToLight)

      let pdf = pdf' / (radius * radius)

      let pdfOther =
            ( pdfSampleCosinusLobe
                roughness
                ( abs $
                    dot (reflect normal wi) (normalize directionToLight)
                )
            )
              / (normSquared directionToLight)
              * (abs (dot (normalize (center --> pointOnLight)) (normalize directionToLight)))

      let sq x = x * x
      let weight = sq pdf / (sq pdf + sq pdfOther)

      pure (x --> pointOnLight, weight * (cosFactor / pi) / (pdf * 4 * radius * radius))
  let directionToLightNormalized = normalize directionToLight

      -- Glossy shading
      rlFactor = dot (reflect normal wi) directionToLightNormalized ** roughness

      -- Diffuse shading
      coef =
        if sameSide normal (flipDirection wi) directionToLightNormalized
          then abs (dot normal directionToLightNormalized * (roughness + 2) * rlFactor / (2 * pi * lightDistance2))
          else 0

      lightDistance2 = dot directionToLight directionToLight

      -- Trace a ray toward the light source and check for intersection
      canSeeLightSource =
        testRayVisibility
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

  pure $ traceIf isNaN "coefNormLight" coefNormLight .*. visibility .*. Streamray.Light.emission light .*. traceIf isNaN "coef" coef

{-# NOINLINE traceIf #-}
traceIf :: (a -> Bool) -> String -> a -> a
traceIf p message v = unsafePerformIO $ do
  when (p v) $ do
    traceIO message
  pure v

-- | Returns the pixel color associated with a 'Ray'. It performs russian rulette.
radiance :: StatefulGen g m => Scene -> Bool -> Int -> Ray -> Float -> g -> m (Maybe (V3 'Color))
radiance _ _ 5 _ _ _ = pure $ Just (C 0 0 0)
radiance scene lastWasSpecular depth ray pdfW g = do
  r <- uniformF g

  let coefRR = if depth <= 1 then 1 else 0.5

  if r < coefRR
    then fmap (((1 :: Float) / coefRR) .*.) <$> subRadiance scene lastWasSpecular depth ray pdfW g
    else pure $ Just (C 0 0 0)

-- | Returns the pixel color associated with a 'Ray'. This is the same as
-- 'radiance', but it does not perform russian rulette.
subRadiance :: forall g m. StatefulGen g m => Scene -> Bool -> Int -> Ray -> Float -> g -> m (Maybe (V3 'Color))
subRadiance scene lastWasSpecular depth ray pdfW g = case rayIntersect ray (objects scene) of
  Nothing -> pure Nothing
  Just (Intersection (AttachedMaterial (Material albedo behavior e) (IntersectionFrame normal x)) t) -> do
    let -- Mirror contribution may be used in multiple material path (Mirror
        -- and Glass), so we factorize the code here.
        mirrorContribution = do
          let reflectedDirection = reflect normal (direction ray)
              reflectedRay = Ray (x .+. epsilon .*. reflectedDirection) reflectedDirection
          contrib <- radiance scene True (depth + 1) reflectedRay 0 g
          pure $ fromMaybe (C 0 0 0) contrib

        visibleIndirect =
          case e of
            Just (Light (SphereLight (Sphere sphereCenter radius)) emission) ->
              let pdf = pdfW * abs (dot normal (direction ray)) / (t * t)

                  pdfOther = pdfSamplingCosinusMax cosThetaMax cosTheta / (radius * radius)
                  cosThetaMax = radius / norm (sphereCenter --> origin ray)

                  cosTheta = abs $ dot (normalize (sphereCenter --> x)) (direction ray)

                  weight :: Float = pdf / (pdfOther + pdf)
               in (if lastWasSpecular then 1 else weight) .*. emission .*. abs (dot normal (direction ray))
            _ -> C 0 0 0
        postLight c = albedo .*. c .+. visibleIndirect

    Just . postLight <$> case behavior of
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
                fromMaybe (C 0 0 0) <$> radiance scene True (depth + 1) transmittedRay 0 g
              else -- This choice was picked with PDF = 1 - coef, and is weighted by 1 - coef (the transmission factor). Both cancels
                mirrorContribution
      Mirror -> mirrorContribution
      Glossy roughness -> do
        -- Sample uniformly one light
        -- TODO: we would like to do that by importance
        lu <- uniformRM (0, length (lights scene) - 1) g
        directLightContrib' <- directLighting scene (direction ray) x normal (lights scene !! lu) roughness g
        let directLightContrib = (fromIntegral (length (lights scene)) :: Float) .*. directLightContrib'

        -- Indirect lighting
        uv <- uniform2F g
        -- Sample a direction proportional to cosinus
        let (pdf, indirectDirection) = rotateVector trueReflectDirection <$> sampleCosinusLobe roughness uv

            coefIndirect = dot indirectDirection (flipDirection normal) / (2 * pi * pdf) * (roughness + 2) * rlFactor

            -- Glossy shading
            trueReflectDirection = reflect (flipDirection normal) (direction ray)
            rlFactor = abs $ dot trueReflectDirection indirectDirection ** roughness

            indirectRay = Ray (x .+. epsilon .*. indirectDirection) indirectDirection

        contribIndirect <- fromMaybe (C 0 0 0) <$> radiance scene False (depth + 1) indirectRay pdf g

        pure $ directLightContrib .+. (coefIndirect .*. contribIndirect)

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
  let contrib = foldl' safeAddContrib (C 0 0 0) contribs

      safeAddContrib c0 c1@(C x' y' z')
        | isNaN x' || isNaN y' || isNaN z' = traceShow "NaN ended in the buffer" c0
        | otherwise = c0 .+. c1

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
      radiance scene True 0 ray 0 g

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
