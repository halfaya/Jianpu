{-# LANGUAGE
  ScopedTypeVariables,
  NamedFieldPuns,
  RecordWildCards,
  OverloadedRecordDot
#-}

module Music where

type Step     = String
type Alter    = Int
type Octave   = Int
type Fifths   = Int
type Rel      = Int  -- relative pitch 0 to 12 where 0 = C

data NoteType = Whole | Half | Quarter | Eighth | Sixteenth | Thirtysecond
  deriving Eq
data NoteMod  = Dotted | Chord | GraceStart | GraceEnd | ModNone
  deriving Eq
data Slur     = SlurStart | SlurStop | SlurStopStart | SlurNone
  deriving Eq
data Tuplet   = TupletStart | TupletStop | TupletNone
  deriving Eq

data Number    = Number {num :: Int, alt :: Alter, oct :: Octave}
data NumRest   = Num Number | Rest
data Note      = Note {noteType :: NoteType,
                       noteMod :: NoteMod,
                       slur :: Slur,
                       tuplet :: Tuplet,
                       numr :: NumRest}

parseNoteType :: String -> NoteType
parseNoteType "whole"   = Whole
parseNoteType "half"    = Half
parseNoteType "quarter" = Quarter
parseNoteType "eighth"  = Eighth
parseNoteType "16th"    = Sixteenth
parseNoteType "32nd"    = Thirtysecond
parseNoteType _         = undefined

alterToRel :: Alter -> Rel -> Rel
alterToRel a p = (p + a) `mod` 12

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
stepAlterToRel s a = alterToRel a (stepToRel s)

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
-}

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

-- Key of D
fixOctave :: Step -> Octave -> Octave
fixOctave s o = o - 4 - (if s == "C" then 1 else 0)

pitchToNumber :: Step -> Alter -> Octave -> Number
pitchToNumber s a o =
  let (n, a') = relToNumber (stepAlterToRel s a)
  in Number {num = n, alt = a', oct = fixOctave s o}

showAlter :: Alter -> String
showAlter 0  = ""
showAlter (-1) = "b"
showAlter 1  = "#"
showAlter _  = undefined

showOctave :: Octave -> String
showOctave o =
  if o >= 0 then replicate o '\''
  else replicate (- o) ','

showNumber :: Number -> String
showNumber Number{..} = showAlter alt ++ show num ++ showOctave oct

showNumRest :: NumRest -> String
showNumRest (Num n) = showNumber n
showNumRest Rest    = "0"

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

showNotePrefix :: Note -> String
showNotePrefix Note{..} = snp1 ++ snp2 ++ snp3 where
  snp1 = case tuplet of
    TupletStart -> "3[ "
    _           -> ""
  snp2 = case noteMod of
    Chord -> ""
    _     -> " "
  snp3 = case noteType of
    Eighth       -> "q"
    Sixteenth    -> "s"
    Thirtysecond -> "d"
    _            -> ""

showNoteSuffix :: Note -> String
showNoteSuffix n@Note{..} = sns1 ++ showDashes n ++ sns2 where
  sns1 = case slur of
    SlurStart     -> " ( "
    SlurStop      -> " )"
    SlurStopStart -> " ) ( "
    _             -> ""
  sns2 = case tuplet of
    TupletStop -> " ]"
    _          -> ""

showDotted :: Note -> String
showDotted n = if n.noteMod /= Dotted then "" else
  case n.noteType of
    Whole -> ""
    Half  -> ""
    _     -> "."

showNote :: Note -> String
showNote n = showNotePrefix n ++ showNumRest n.numr ++ showDotted n ++ showNoteSuffix n

aaa = showNumber (pitchToNumber "C" 0 4)
