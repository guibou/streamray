{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Streamray.Geometry.Triangle where

import Codec.Wavefront hiding (Triangle)
import Control.DeepSeq
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debug.Trace
import GHC.Generics
import Streamray.Geometry.Box
import Streamray.Linear

data Triangle = Triangle {-# UNPACK #-} !(V3 'Position) {-# UNPACK #-} !(V3 'Position) {-# UNPACK #-} !(V3 'Position)
  deriving (Show, NFData, Generic)

readTriangles :: FilePath -> IO (Vector Triangle)
readTriangles path = do
  Right asset <- fromFile path

  let positions = do
        Location x z y w <- objLocations asset
        pure $ P (x / w) (y / w) (z / w)

  print $ Vector.foldl' minP (Vector.head positions) positions
  print $ Vector.foldl' maxP (Vector.head positions) positions

  let readPoint (faceLocIndex -> idx) = positions Vector.! (idx - 1)
  let res = do
        (elValue -> Face x y z []) <- objFaces asset
        pure $ Triangle (readPoint x) (readPoint y) (readPoint z)
  pure $ traceShow (Vector.length res) res

instance HasBoundingBox Triangle where
  toBox (Triangle p0 p1 p2) = buildBox p0 p1 `addPointToBox` p2
