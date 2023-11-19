module Main where

import Protolude as P

import Data.List.Index (imap, setAt)
import Data.Map.Strict as Map
import Data.Text as Text

import GeneralTypes (
  Interval (..),
  MidiNote (..),
 )

data Finger = Thumb | Index | Middle | Ring | Pinky | AnyFinger
  deriving (Eq, Ord, Show)

fingerToText :: Finger -> Text
fingerToText finger =
  let colorize text = "\x1b[31m" <> text <> "\x1b[0m"
   in colorize $ case finger of
        Thumb -> "T"
        Index -> "I"
        Middle -> "M"
        Ring -> "R"
        Pinky -> "P"
        AnyFinger -> "●"

data FretPosition
  = F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | FA
  | FB
  | FC
  | FD
  | FE
  | FF
  | FG
  | FH
  | FI
  | FJ
  | FK
  | FL
  | FM
  | FN
  | FO
  | FP
  | FQ
  | FR
  | FS
  | FT
  | FU
  | FV
  | FW
  | FX
  | FY
  | FZ
  deriving (Bounded, Enum, Eq, Ord, Show)

data Pick
  = Mute
  | Open
  | Pick FretPosition Finger
  deriving (Eq, Ord, Show)

pickToInt :: Pick -> Int
pickToInt fretPosition =
  case fretPosition of
    Pick fret _ -> (fromEnum fret) + 1
    _ -> 0

type Fretting = [[Pick]]

type InstStrings = [Interval]
data PlayedInstrument = PlayedInst InstStrings MidiNote Fretting
type Instrument = Fretting -> PlayedInstrument

ukulele :: Instrument
ukulele =
  PlayedInst [I07, I00, I04, I09] M34

gMajor :: PlayedInstrument
gMajor =
  ukulele
    [ [Open]
    , [Pick F2 Index]
    , [Pick F3 Ring]
    , [Pick F2 Middle]
    ]

--     G
--  ╒═╤═╤═╕
--  │_│_│_│
--  │_I_│_M
--  │_│_R_│
--  │_│_│_│

bMajor :: PlayedInstrument
bMajor =
  ukulele
    [
      [ Pick F2 Index
      , Pick F4 Ring
      ]
    ,
      [ Pick F2 Index
      , Pick F3 Middle
      ]
    , [Pick F2 Index]
    , [Pick F2 Index]
    ]

--     G
--  ╒═╤═╤═╕
--  │_│_│_│
--  I_I_I_I
--  │_M_│_│
--  R_│_│_│
--  │_│_│_│

archaicToFrettingA :: Map Text [Fretting]
archaicToFrettingA =
  Map.fromList
    [
      ( "a"
      ,
        [ [[Pick F2 Middle], [Pick F1 Index], [Open], [Open]]
        , [[Pick F2 Middle], [Pick F1 Index], [Open], [Pick F4 Pinky]]
        ]
      )
    ,
      ( "am"
      ,
        [ [[Pick F2 Middle], [Open], [Open], [Open]]
        , [[Pick F2 Middle], [Open], [Open], [Pick F3 AnyFinger]]
        ]
      )
    ]

chordToPlayedInsts ::
  Text -> Instrument -> Either Text [PlayedInstrument]
chordToPlayedInsts chord instrument =
  let
    maybeInst = do
      frettings <- Map.lookup chord archaicToFretting
      pure $ fmap instrument frettings
    errorMessage =
      "There is no fretting available for the specified chord"
   in
    maybeToEither errorMessage maybeInst

putPickOnString :: Pick -> [Text] -> [Text]
putPickOnString pick stringParts =
  case pick of
    Mute -> stringParts
    Open -> stringParts
    (Pick _ finger) ->
      setAt
        (pickToInt pick)
        (fingerToText finger)
        stringParts

getString :: Int -> Int -> Int -> [Pick] -> [Text]
getString numberOfFrets numOfStrings stringIndex strPick =
  let
    openString =
      [ ( if
            | stringIndex == 0 -> "╒"
            | stringIndex == (numOfStrings - 1) -> "╕"
            | otherwise -> "╤"
        )
      ]
        <> P.replicate (numberOfFrets + 1) "│"
   in
    P.foldr putPickOnString openString strPick

showFretting :: Fretting -> Text
showFretting fretting =
  let
    maxPos = P.maximum $ fmap pickToInt $ fold fretting
   in
    fretting
      & imap (getString maxPos $ P.length fretting)
      & P.intersperse (["═"] <> P.replicate (maxPos + 1) "_")
      & P.transpose
      & P.intercalate ["\n"]
      & fold
      & (<> "\n")

showPlayedInst :: PlayedInstrument -> Either Text Text
showPlayedInst (PlayedInst strings _ fretting)
  | P.length strings /= P.length fretting =
      Left "Number of strings and picks in fretting do not match"
  | otherwise =
      Right
        $ showFretting fretting

getAnsiArts :: Text -> Either Text Text
getAnsiArts chord = do
  playedInsts <- chordToPlayedInsts chord ukulele
  fmap
    (Text.intercalate "\n")
    (mapM showPlayedInst playedInsts)

archaicToFretting :: Map Text [Fretting]
archaicToFretting =
  Map.fromList
    [
      ( "a"
      ,
        [ [[Pick F2 Middle], [Pick F1 Index], [Open], [Open]]
        ,
          [ [Pick F4 Index, Pick F6 Ring]
          , [Pick F4 Index]
          , [Pick F4 Index, Pick F5 Middle]
          , [Pick F4 Index]
          ]
        ]
      )
    ,
      ( "am"
      ,
        [ [[Pick F2 Middle], [Open], [Open], [Open]]
        ]
      )
    ,
      ( "a#"
      ,
        [
          [ [Pick F1 Index, Pick F3 Ring]
          , [Pick F1 Index, Pick F2 Middle]
          , [Pick F1 Index]
          , [Pick F1 Index]
          ]
        ]
      )
    ,
      ( "a#m"
      ,
        [
          [ [Pick F1 Index, Pick F3 Ring]
          , [Pick F1 Index]
          , [Pick F1 Index]
          , [Pick F1 Index]
          ]
        ]
      )
    ,
      ( "b"
      ,
        [
          [ [Pick F2 Index, Pick F4 Ring]
          , [Pick F2 Index, Pick F3 Middle]
          , [Pick F2 Index]
          , [Pick F2 Index]
          ]
        ]
      )
    ,
      ( "bm"
      ,
        [
          [ [Pick F2 Index, Pick F4 Ring]
          , [Pick F2 Index]
          , [Pick F2 Index]
          , [Pick F2 Index]
          ]
        ]
      )
    , ("c", [[[Open], [Open], [Open], [Pick F3 Ring]]])
    , ("cm", [[[Open], [Pick F3 Index], [Pick F3 Index], [Pick F3 Index]]])
    ,
      ( "c#"
      ,
        [
          [ [Pick F1 Index]
          , [Pick F1 Index]
          , [Pick F1 Index]
          , [Pick F1 Index, Pick F4 Pinky]
          ]
        ]
      )
    , ("c#m", [[[Pick F1 Index], [Pick F1 Index], [Open], [Open]]])
    , ("d", [[[Pick F2 Index], [Pick F2 Middle], [Pick F2 Middle], [Open]]])
    , ("dm", [[[Pick F2 Middle], [Pick F2 Ring], [Pick F1 Index], [Open]]])
    , ("d#", [[[Open], [Pick F3 Ring], [Pick F3 Pinky], [Pick F1 Index]]])
    ,
      ( "d#m"
      ,
        [ [[Pick F3 Ring], [Pick F3 Pinky], [Pick F2 Middle], [Pick F1 Index]]
        ]
      )
    , ("e", [[[Pick F1 Index], [Pick F4 Pinky], [Open], [Pick F2 Middle]]])
    , ("em", [[[Open], [Pick F4 Ring], [Pick F3 Middle], [Pick F2 Index]]])
    , ("f", [[[Pick F2 Middle], [Open], [Pick F1 Index], [Open]]])
    , ("fm", [[[Pick F1 Index], [Open], [Pick F1 Middle], [Pick F3 Pinky]]])
    ,
      ( "f#"
      ,
        [
          [ [Pick F1 Index, Pick F3 Ring]
          , [Pick F1 Index]
          , [Pick F1 Index, Pick F2 Middle]
          , [Pick F1 Index]
          ]
        ]
      )
    , ("f#m", [[[Pick F2 Middle], [Pick F1 Index], [Pick F2 Ring], [Open]]])
    , ("g", [[[Open], [Pick F2 Index], [Pick F3 Ring], [Pick F2 Middle]]])
    , ("gm", [[[Open], [Pick F2 Middle], [Pick F3 Ring], [Pick F1 Index]]])
    ,
      ( "g#"
      ,
        [
          [ [Pick F3 Index, Pick F5 Ring]
          , [Pick F3 Index]
          , [Pick F3 Index, Pick F4 Middle]
          , [Pick F3 Index]
          ]
        ]
      )
    ,
      ( "g#m"
      ,
        [ [[Pick F4 Ring], [Pick F3 Middle], [Pick F4 Pinky], [Pick F2 Index]]
        ]
      )
    ]

main :: IO ()
main = do
  chords <- getArgs
  case chords of
    [] -> die "Usage: uku <chord>"
    [chord] -> case (getAnsiArts $ pack chord) of
      Left error -> die error
      Right ansiArt -> putStr $ ansiArt <> "\n"
    _ -> die "Supportrs only 1 chord per call"