{-|
Description : Main entry point for the CLI app.

Display ukulele fingering charts in your terminal.
-}
module Main where

import Protolude as P (
  Either (..),
  IO,
  Print (putStr),
  Semigroup ((<>)),
  die,
  getArgs,
  ($),
 )

import Data.Text as Text (pack)

import Uku.Render (getAnsiArts)


main :: IO ()
main = do
  chords <- getArgs
  case chords of
    [] -> die "Usage: uku <chord>"
    [chord] -> case getAnsiArts $ pack chord of
      Left error -> die error
      Right ansiArt -> putStr $ ansiArt <> "\n"
    _ -> die "Supports only 1 chord per invocation"
