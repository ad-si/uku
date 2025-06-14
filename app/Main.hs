{-|
Description : Main entry point for the CLI app.

Display ukulele fingering charts in your terminal.
-}
module Main where

import Protolude as P (
  Applicative (pure),
  Char,
  Either (..),
  Functor (fmap),
  IO,
  Int,
  Print (putStr),
  Semigroup ((<>)),
  die,
  getArgs,
  length,
  mapM,
  maximum,
  replicate,
  toUpper,
  transpose,
  zipWith,
  ($),
  (&),
  (+),
  (-),
  (.),
  (<$>),
  (<&>),
 )

import Data.Text qualified as Text (
  Text,
  intercalate,
  lines,
  pack,
  replicate,
  unlines,
  unpack,
 )

import Uku.Render (getAnsiArts)


toArt :: [Char] -> IO (Text.Text, Text.Text)
toArt raw = do
  let displayName = Text.pack (capitalizeChord raw)
  case getAnsiArts (Text.pack raw) of
    Left err -> die err
    Right art -> pure (displayName, art)


-- | Capitalize the first letter only (A, B♭, G#m, …)
capitalizeChord :: [Char] -> [Char]
capitalizeChord [] = []
capitalizeChord (x : xs) = toUpper x : xs


-- | Visible length (ignores "\ESC[ … m" sequences).
visibleLen :: Text.Text -> Int
visibleLen = do
  let
    skip [] = []
    skip ('m' : ys) = ys -- End of CSI
    skip (_ : ys) = skip ys

    go n [] = n
    go n ('\x1b' : xs) = go n $ skip xs -- Skip CSI
    go n (_ : xs) = go (n + 1) xs

  go 0 . Text.unpack


combineArts :: [(Text.Text, Text.Text)] -> Text.Text
combineArts arts = do
  let
    -- Prepend the chord name to every column
    colsLines = fmap (\(name, art) -> name : Text.lines art) arts

    -- Pad every column to the same number of rows
    maxRows = P.maximum (P.length <$> colsLines)
    padRows x = x <> P.replicate (maxRows - P.length x) ""
    rowPadded = fmap padRows colsLines

    -- Width of every column
    widths = fmap (P.maximum . fmap visibleLen) rowPadded
    -- Right-pad every cell to the column width
    colPadded =
      P.zipWith
        (\col w -> col <&> (\l -> l <> Text.replicate (w - visibleLen l) " "))
        rowPadded
        widths
    -- Combine the rows horizontally
    joined = colPadded & P.transpose <&> Text.intercalate "  "

  Text.unlines joined


main :: IO ()
main = do
  chords <- getArgs
  case chords of
    [] -> die "Usage: uku <chord> [<chord> ...]"
    cs -> do
      arts <- mapM toArt cs
      putStr $ combineArts arts
