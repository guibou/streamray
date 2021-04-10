{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Options.Applicative
import Streamray.Render
import Streamray.RenderSettings

configParser :: Parser RenderSettings
configParser = do
  samplesPerPixel <-
    option
      auto
      ( long "samples-per-pixel"
          <> short 's'
          <> help "Number of samples per pixel"
          <> showDefault
          <> value 10
          <> metavar "SAMPLES"
      )
  knownScene <-
    option
      auto
      ( long "scene"
          <> showDefault
          <> value Cornel
          <> metavar "SCENE-EXPR"
      )
  filepath <-
    option
      str
      ( long "output"
          <> short 'o'
          <> showDefault
          <> value "first_image.png"
          <> metavar "FILENAME.png"
      )

  pure
    RenderSettings
      { samplesPerPixel,
        knownScene,
        filepath
      }

opts :: ParserInfo RenderSettings
opts =
  info
    (configParser <**> helper)
    ( fullDesc
        <> progDesc "Streamray does ray tracing"
        <> header "A program to raytrace your scene"
    )

main :: IO ()
main = do
  config <- execParser opts

  print config

  raytraceImage config
