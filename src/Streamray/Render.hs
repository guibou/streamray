{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | This is the core of the rendering algorithm, with the main raytrace
-- "integrator", called 'radiance', as well as a naive camera model and image
-- saving.
module Streamray.Render where

import Codec.Picture
import Control.Monad (replicateM)
import Data.Foldable
import Data.Maybe
import Streamray.Linear
import Streamray.Material
import Streamray.Ray
import Streamray.Sampling
import Streamray.Scene
import System.Random

-- | Returns the pixel color associated with a 'Ray'
radiance :: Int -> Ray -> IO (Maybe (V3 'Color))
radiance 5 _ = pure $ Just (C 0 0 0)
radiance depth ray = do
  r <- randomIO @Float

  let coefRR = if depth == 0 then 1 else 0.5

  if r < coefRR
    then fmap (((1 :: Float) / coefRR) .*.) <$> subRadiance depth ray
    else pure $ Just (C 0 0 0)

subRadiance :: Int -> Ray -> IO (Maybe (V3 'Color))
subRadiance depth ray = case rayIntersectObjets ray (objects scene) of
  Nothing -> pure Nothing
  Just (Intersection (Object (Material albedo behavior) sphere) t) -> do
    let x = origin ray .+. t .*. direction ray
        normal = normalize (center sphere --> x)

        -- Mirror contribution may be used in multiple material path (Mirror
        -- and Glass), so we factorize the code here.
        mirrorContribution = do
          let reflectedDirection = reflect normal (direction ray)
              reflectedRay = Ray (x .+. epsilon .*. reflectedDirection) reflectedDirection
          contrib <- radiance (depth + 1) reflectedRay
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
            (r :: Float) <- randomIO

            -- Pick a random choice, with probability (PDF) = coef
            if r < coef
              then do
                -- This choice was picked with PDF = coef, and is weighted by coef (the transmission factor). Both cancels
                let transmittedRay = Ray (x .+. (epsilon * 3) .*. transmittedDirection) transmittedDirection
                fromMaybe (C 0 0 0) <$> radiance (depth + 1) transmittedRay
              else -- This choice was picked with PDF = 1 - coef, and is weighted by 1 - coef (the transmission factor). Both cancels
                mirrorContribution
      Mirror -> mirrorContribution
      Diffuse -> do
        let -- TODO: handle surface factors
            directionToLight = x --> position (lights scene !! 0)
            directionToLightNormalized = normalize directionToLight
            coef = max 0 (dot normal directionToLightNormalized / (pi * lightDistance2))

            lightDistance2 = dot directionToLight directionToLight

            -- Trace a ray toward the light source and check for intersection
            canSeeLightSource = testRayVisibility
              ( Ray
                  -- The origin of the ray is biased toward the light to avoid self
                  -- shadows if the point is slightly under the surface due to
                  -- floating point approximations.
                  ( x
                      .+. epsilon .*. directionToLightNormalized
                  )
                  directionToLightNormalized
              )
              (objects scene) lightDistance2

            visibility = if canSeeLightSource then C 1 1 1 else C 0 0 0

            directLightContrib = visibility .*. emission (lights scene !! 0) .*. coef

        -- Indirect lighting
        u <- randomIO @Float
        v <- randomIO @Float
        -- Sample a direction proportional to cosinus
        let (_pdf, indirectDirection) = rotateVector normal <$> sampleCosinus u v

            -- The surface value is cos / pi, which is equal to the pdf. So they cancels.
            -- coefIndirect = (dot indirectDirection normal / pi) / pdf
            coefIndirect :: Float = 1
            indirectRay = Ray (x .+. epsilon .*. indirectDirection) indirectDirection

        contribIndirect <- fromMaybe (C 0 0 0) <$> radiance (depth + 1) indirectRay

        pure $ directLightContrib .+. (coefIndirect .*. contribIndirect)

sameSide :: V3 ('Direction k) -> V3 ('Direction k'1) -> V3 ('Direction k'2) -> Bool
sameSide n v0 v1 = (dot n v0 * dot n v1) > 0

-- | Changes the ray offset used to escape surface
epsilon :: Float
epsilon = 0.01

-- | Convert a light measure to a pixel value
tonemap :: Float -> V3 'Color -> PixelRGBA8
tonemap alpha v = PixelRGBA8 x y z (truncate . max 0 . min 255 . (* 255) $ alpha)
  where
    -- truncate converts to Word8
    -- min/max clamps to the acceptable range
    -- pow (1 / 2.2) is doing gamma correction
    C (ftonemap -> x) (ftonemap -> y) (ftonemap -> z) = v

-- | Converts a value from a measure to the RGB colorspace in 8bit.
ftonemap :: Float -> Pixel8
-- truncate converts to Word8
-- min/max clamps to the acceptable range
-- pow (1 / 2.2) is doing gamma correction
ftonemap = truncate @Float @Pixel8 . max 0 . min 255 . (* 255) . (** (1 / 2.2))

-- | Raytrace a 500x500 image
-- This function is called for each pixel
raytrace :: Int -> Int -> IO PixelRGBA8
raytrace (fromIntegral -> x) (fromIntegral -> y) = do
  let nSamples = 10
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
      dx <- randomIO @Float
      dy <- randomIO @Float
      let -- Generate a ray in the XY plane and pointing in the Z direction
          coefOpening = 1.001

          -- TODO. Discuss about proper filtering
          n = P (x + dx) (y + dy) 0

          -- n' is on the plane [-250:250]
          n'@(P x' y' 0) = n .-. D 250 250 0
          f = P (coefOpening * x') (coefOpening * y') 1

          d = normalize (n' --> f)
          ray = Ray n d
      radiance 0 ray

-- | Raytrace a 500x500 image, using the default scene, and saves it.
raytraceImage :: FilePath -> IO ()
raytraceImage path = writePng path =<< withImage 500 500 raytrace
