{-# LANGUAGE
  ScopedTypeVariables,
  NamedFieldPuns,
  RecordWildCards,
  OverloadedRecordDot
#-}

module Music where

import Data.List (intercalate)

type Step     = String
type Alter    = Int
type Octave   = Int
type Rel      = Int  -- relative pitch 0 to 12 where 0 = C

data Score = Score {
  sTitle      :: String,
  sComposer   :: String,
  sInstrument :: String,
  sMeasures   :: [Measure] }

data Key = KeyD | KeyG | KeyA | KeyE | KeyF
  deriving Eq
data Time = Time String String -- numerator and denominator
  deriving Eq
data Tempo = Tempo NoteType String -- latter is beats per minutea
  deriving Eq

data Measure = Measure {
  mNumber :: String,
  mEmpty  :: Int, -- represents this many empty measures
  mKey    :: Maybe Key,
  mTime   :: Maybe Time,
  mTempo  :: Maybe Tempo,
  mNotes  :: [Note] }

data NoteType = Whole | Half | Quarter | Eighth | Sixteenth | Thirtysecond
  deriving Eq
data NoteMod  = Dotted | Chord | Grace | ModNone
  deriving Eq
data Slur     = SlurStart | SlurStop | SlurStopStart | SlurNone
  deriving (Eq, Show)
data Tie      = TieStart | TieStop | TieNone
  deriving Eq
data Tuplet   = TupletStart | TupletStop | TupletNone
  deriving Eq

data Number    = Number {step :: Step, alt :: Alter, oct :: Octave}
  deriving Eq
data NumRest   = Num Number | Rest
  deriving Eq

data Note      = Note {
  noteType :: NoteType,
  noteMod  :: NoteMod,
  noteDir  :: Direction,
  slur     :: Slur,
  tie      :: Tie,
  tuplet   :: Tuplet,
  numr     :: NumRest }

data Direction = Dynamic String | DirNone

parseNoteType :: String -> NoteType
parseNoteType "whole"   = Whole
parseNoteType "half"    = Half
parseNoteType "quarter" = Quarter
parseNoteType "eighth"  = Eighth
parseNoteType "16th"    = Sixteenth
parseNoteType "32nd"    = Thirtysecond
parseNoteType _         = undefined

noteTypeToInt :: NoteType -> String
noteTypeToInt Whole        = "1"
noteTypeToInt Half         = "2"
noteTypeToInt Quarter      = "4"
noteTypeToInt Eighth       = "8"
noteTypeToInt Sixteenth    = "16"
noteTypeToInt Thirtysecond = "32"

stepToRel :: Step -> Rel
stepToRel "C" = 0
stepToRel "D" = 2
stepToRel "E" = 4
stepToRel "F" = 5
stepToRel "G" = 7
stepToRel "A" = 9
stepToRel "B" = 11
stepToRel _   = undefined

stepAlterToRel :: Step -> Alter -> Rel
stepAlterToRel s a = stepToRel s + a

-- Reduce octave so middle C is at C0
numberToAbsolute :: Number -> Int
numberToAbsolute Number{..} = stepAlterToRel step alt + (oct - 4) * 12



{-
-- key of C
relToNumber :: Rel -> Number
relToNumber 0  = (1, 0)
relToNumber 1  = (1, 1)
relToNumber 2  = (2, 0)
relToNumber 3  = (2, 1)
relToNumber 4  = (3, 0)
relToNumber 5  = (4, 0)
relToNumber 6  = (4, 1)
relToNumber 7  = (5, 0)
relToNumber 8  = (5, 1)
relToNumber 9  = (6, 0)
relToNumber 10 = (6, 1)
relToNumber 11 = (7, 0)

--key of D
relToNumber :: Rel -> (Int, Alter)
relToNumber 0  = (7, -1)
relToNumber 1  = (7, 0)
relToNumber 2  = (1, 0)
relToNumber 3  = (1, 1)
relToNumber 4  = (2, 0)
relToNumber 5  = (3, -1)
relToNumber 6  = (3, 0)
relToNumber 7  = (4, 0)
relToNumber 8  = (4, 1)
relToNumber 9  = (5, 0)
relToNumber 10 = (5, 1)
relToNumber 11 = (6, 0)
-}

relToNumberD :: Rel -> (Int, Alter)
relToNumberD 0  = (1, 0) -- D
relToNumberD 1  = (1, 1) -- D#
relToNumberD 2  = (2, 0) -- E
relToNumberD 3  = (3, -1) -- F
relToNumberD 4  = (3, 0) -- F#
relToNumberD 5  = (4, 0) -- G
relToNumberD 6  = (4, 1) -- G#
relToNumberD 7  = (5, 0) -- A
relToNumberD 8  = (5, 1) -- A#
relToNumberD 9  = (6, 0) -- B
relToNumberD 10 = (7, -1) -- C
relToNumberD 11 = (7, 0) -- C#

relToNumberF :: Rel -> (Int, Alter)
relToNumberF 0  = (1, 0) -- F
relToNumberF 1  = (2, -1) -- Gb
relToNumberF 2  = (2, 0) -- G
relToNumberF 3  = (3, -1) -- Ab
relToNumberF 4  = (3, 0) -- A
relToNumberF 5  = (4, 0) -- Bb
relToNumberF 6  = (4, 1) -- B
relToNumberF 7  = (5, 0) -- C
relToNumberF 8  = (6, -1) -- Db
relToNumberF 9  = (6, 0) -- D
relToNumberF 10 = (7, -1) -- Eb
relToNumberF 11 = (7, 0) -- E

-- number of half steps to adjust to get the tonic note for the key into
-- relative position 0
adjustKey :: Key -> Int
adjustKey KeyD = -2
adjustKey KeyE = -4
adjustKey KeyF = -5
adjustKey KeyG = -7
adjustKey KeyA = -9

pitchToNumber :: Step -> Alter -> Octave -> Number
pitchToNumber s a o = Number {step = s, alt = a, oct = o}

showAlter :: Alter -> String
showAlter 0  = ""
showAlter (-1) = "b"
showAlter 1  = "#"
showAlter _  = undefined

showOctave :: Octave -> String
showOctave o =
  if o >= 0 then replicate o '\''
  else replicate (- o) ','

showNumber :: Key -> Number -> String
showNumber key num =
  let abs = numberToAbsolute num + adjustKey key
      oct = abs `div` 12
      rel = abs `mod` 12
      (r, a) = case key of
        KeyD -> relToNumberD rel
        KeyF -> relToNumberD rel
        _    -> undefined
  in showOctave oct ++ showAlter a ++ show r

showNumRest :: Key -> NumRest -> String
showNumRest key (Num n) = showNumber key n
showNumRest _   Rest    = "0"

showDashes :: Note -> String
showDashes Note{..} =
  let c = case numr of
        Num _ -> " -"
        Rest  -> " 0"
      i = case (noteType, noteMod) of
        (Whole, Dotted) -> 5
        (Whole, _)      -> 3
        (Half, Dotted)  -> 2
        (Half, _      ) -> 1
        (_,_)           -> 0
  in concat (replicate i c)

showDir :: Note -> String
showDir n = case n.noteDir of
  Dynamic d -> " \\" ++ d
  _         -> ""

showNotePrefix :: Note -> String
showNotePrefix Note{..} = snp1 where
  snp1 = case tuplet of
    TupletStart -> " 3[ "
    _           -> " "

showNoteSuffix :: Note -> String
showNoteSuffix n@Note{..} = sns1 ++ showDir n ++ showDashes n ++ sns2 ++ sns3 where
  sns1 = case slur of
    SlurStart     -> " ( "
    SlurStop      -> " )"
    SlurStopStart -> " ) ( "
    _             -> ""
  sns2 = case tuplet of
    TupletStop -> " ]"
    _          -> ""
  sns3 = case tie of
    TieStart -> " ~"
    _        -> ""

showDotted :: Note -> String
showDotted n = if n.noteMod /= Dotted then "" else
  case n.noteType of
    Whole -> ""
    Half  -> ""
    _     -> "."

isGrace :: Note -> Bool
isGrace n = n.noteMod == Grace

groupChords :: [Note] -> [[Note]]
groupChords ns = filter (not . null) (gc [] ns) where
  gc :: [Note] -> [Note] -> [[Note]]
  gc acc [] = [acc]
  gc acc (x : []) = if x.noteMod == Chord then [acc ++ [x]] else [acc, [x]]
  gc acc (x : y : xs) = case (x.noteMod == Chord, y.noteMod == Chord) of
    (False, False) -> [acc , [x]] ++ gc [y] xs
    (False, True)  -> acc : gc [x, y] xs
    (True, _)      -> gc (acc ++ [x]) (y : xs)

showNoteType :: NoteType -> String
showNoteType Eighth       = "q"
showNoteType Sixteenth    = "s"
showNoteType Thirtysecond = "d"
showNoteType  _           = ""

showBaseNote :: Key -> Note -> String
showBaseNote key n = showNoteType n.noteType ++ showNumRest key n.numr ++ showDotted n

showBaseChord :: Key -> [Note] -> String
showBaseChord key ns = concatMap (showBaseNote key) (reverse ns)

-- grace is true if previous note was grace note
-- second return argument is true if this note is a grace note
showChord :: Key -> Bool -> [Note] -> (String, Bool)
showChord key _     []  = undefined
showChord key grace (n : ns) = case (grace, isGrace n) of
  (False, False) -> (s, False)
  (False, True)  -> (" g[" ++ showNumRest key n.numr, True) -- ns must be []
  (True,  False) -> ("]" ++ s, False)
  (True,  True)  -> (showNumRest key n.numr, True) -- ns must be []
  where s = showNotePrefix n ++ showBaseChord key (n : ns) ++ showNoteSuffix n

showChords :: Key -> [[Note]] -> String
showChords key xs = sn False xs where
  sn _ [] = ""
  sn b (x : xs) = let (s, b') = showChord key  b x in s ++ sn b' xs

showKey :: Maybe Key -> String
showKey Nothing = ""
showKey (Just KeyG) = "1=G\n"
showKey (Just KeyD) = "1=D\n"
showKey (Just KeyA) = "1=A\n"
showKey (Just KeyE) = "1=E\n"
showKey (Just KeyF) = "1=F\n"

showTime :: Maybe Time -> String
showTime Nothing           = ""
showTime (Just (Time n d)) = n ++ "/" ++ d ++ "\n"

showTempo :: Maybe Tempo -> String
showTempo Nothing            = ""
showTempo (Just (Tempo n b)) = noteTypeToInt n ++ "=" ++ b ++ "\n"

hasNoNotes :: Measure -> Bool
hasNoNotes m = all (\n -> n.numr == Rest) m.mNotes

isEmpty :: Measure -> Bool
isEmpty m = hasNoNotes m && m.mKey == Nothing && m.mTime == Nothing && m.mTempo == Nothing

combineEmptyMeasures :: [Measure] -> [Measure]
combineEmptyMeasures ms = cem [] ms where
  cem :: [Measure] -> [Measure] -> [Measure]
  cem acc         []       = f acc
  cem []          (m : ms) = if hasNoNotes m then cem [m] ms else m : cem [] ms
  cem acc@(_ : _) (m : ms) = if isEmpty m
                             then cem (acc ++ [m]) ms
                             else (f acc) ++ cem [] (m:ms)
  f :: [Measure] -> [Measure]
  f [] = []
  f xs@(x : _) = [x {mEmpty = length xs}]

showMeasure :: Key -> Measure -> (String, Key)
showMeasure key Measure{..} =
  let k = case mKey of
            Nothing -> key
            Just k' -> k'
      s = if mEmpty > 1 then "R*" ++ show mEmpty else showChords k (groupChords mNotes)
  in (showKey mKey ++
     showTime mTime ++
     showTempo mTempo ++
     s ++
     (replicate (70 - length s) ' ') ++ "  % " ++ mNumber, k)

-- If key is undefined in the first measure, it will default to D.
showMeasures :: [Measure] -> String
showMeasures ms =
  fst (foldl (\(s,k) m ->
               let (s',k') = showMeasure k m in (s ++ "\n" ++ s', k')) ("", KeyD) ms)

showTag :: String -> String -> String
showTag tag s = tag ++ "=" ++ s ++ "\n"

showScore :: Score -> String
showScore Score{..} =
  showTag "title" sTitle ++
  showTag "composer" sComposer ++
  showTag "arranger" "arr. Emily Wing" ++ -- hardcoded for now
  showTag "instrument" sInstrument ++
  "WithStaff\n" ++                        -- remove to show only jianpu
  showMeasures (combineEmptyMeasures sMeasures)
