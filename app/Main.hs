{-|
Description : Main entry point for the CLI app.

Display ukulele fingering charts in your terminal.
-}
module Main where

import Protolude (
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
  ($),
  (&),
  (+),
  (-),
  (.),
  (<$>),
  (<&>),
 )
import Protolude qualified as P

import Data.Text qualified as Text

import Uku.GeneralTypes (
  addIntervalToKey,
  archaicToKey,
  keyToText,
  nashvilleDegreeToInterval,
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
capitalizeChord (x : xs) = P.toUpper x : xs


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
  let usageText =
        "Usage: uku <chord> [<chord> ...]\n\
        \       uku nash <key> <degree> [<degree> ...]\n\
        \\n\
        \Display ukulele fingering charts in your terminal.\n\
        \\n\
        \Subcommands:\n\
        \  nash    Convert Nashville Number System degrees to chords.\n\
        \          <key> is a root note (e.g. C, Bb, F#).\n\
        \          <degree> is a scale degree from 1 to 7."
  case chords of
    [] -> die usageText
    ["help"] -> die usageText
    ["--help"] -> die usageText
    ["-h"] -> die usageText
    "nash" : keyText : numbers -> do
      let result = do
            numbersInt <-
              P.maybeToEither "ERROR: All numbers must be valid integers" $
                P.mapM (P.readMaybe @Int) numbers
            key <- archaicToKey $ Text.pack keyText
            intervals <-
              P.maybeToEither "ERROR: Nashville numbers must be between 1 and 7" $
                P.mapM nashvilleDegreeToInterval numbersInt
            pure $
              intervals
                <&> (\interval -> Text.unpack $ keyToText $ addIntervalToKey key interval)
      case result of
        Left errMsg -> die errMsg
        Right chordNames -> do
          arts <- P.mapM toArt chordNames
          putStr $ combineArts arts
    cs -> do
      arts <- P.mapM toArt cs
      putStr $ combineArts arts
