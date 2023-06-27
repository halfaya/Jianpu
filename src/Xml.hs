{-# LANGUAGE
  ScopedTypeVariables,
  NamedFieldPuns,
  RecordWildCards,
  OverloadedRecordDot
#-}

module Xml where

import Text.XML.Light
import Text.XML.Light.Output

import Debug.Trace (trace)

import Music

attr :: String -> Element -> String
attr s e = case findAttrBy (\q -> qName q == s) e of
  Just x  -> x
  Nothing -> "NOTFOUND"

children :: String -> Element -> [Element]
children s = filterChildrenName (\q -> qName q == s)

children2 :: String -> String -> Element -> [Element]
children2 s1 s2 = filterChildrenName (\q -> qName q == s1 || qName q == s2)

child :: String -> Element -> Maybe Element
child s e = case (children s e) of
  []      -> Nothing
  (x : _) -> Just x

childR :: [String] -> Element -> Maybe Element
childR []       _ = Nothing
childR [x]      e = child x e
childR (x : xs) e = child x e >>= childR xs

cval :: String -> Element -> String
cval s e = case (child s e) of
  Nothing -> ""
  Just x  -> strContent x

cvalR :: [String] -> Element -> Maybe String
cvalR xs e = fmap strContent (childR xs e)

toInt :: String -> Alter
toInt "" = 0
toInt x  = read x

numRest :: Element -> NumRest
numRest e = case (child "pitch" e) of
  Nothing -> Rest
  Just x  -> Num (number x)

number :: Element -> Number
number e = pitchToNumber (cval "step" e) (toInt (cval "alter" e)) (toInt (cval "octave" e))

chord :: Element -> Bool
chord e = case child "chord" e of
  Nothing -> False
  Just _  -> True

dot :: Element -> Bool
dot e = case child "dot" e of
  Nothing -> False
  Just _  -> True

grace :: Element -> Bool
grace e = case child "grace" e of
  Nothing -> False
  Just _  -> True

getTuplet1 :: Element -> [Tuplet]
getTuplet1 e = case child "tuplet" e of
    Nothing -> []
    Just x -> case attr "type" x of
      "start" -> [TupletStart]
      "stop"  -> [TupletStop]
      _       -> []

getTuplet :: Element -> Tuplet
getTuplet e = case concatMap getTuplet1 (children "notations" e) of
  []            -> TupletNone
  [TupletStart] -> TupletStart
  [TupletStop]  -> TupletStop

getSlur1 :: Element -> [Slur]
getSlur1 e = case child "slur" e of
    Nothing -> []
    Just x -> case attr "type" x of
      "start" -> [SlurStart]
      "stop"  -> [SlurStop]
      _       -> []

getSlur :: Element -> Slur
getSlur e = case concatMap getSlur1 (children "notations" e) of
  []                    -> SlurNone
  [SlurStart]           -> SlurStart
  [SlurStop]            -> SlurStop
  [SlurStop, SlurStart] -> SlurStopStart
  [SlurStart,SlurStart] -> SlurStart
  [SlurStop,SlurStop]   -> SlurStop
  x                     -> trace ("getSlur: " ++ show x) undefined
  
getTie :: Element -> Tie
getTie e = case child "tie" e of
    Nothing -> TieNone
    Just x -> case attr "type" x of
      "start" -> TieStart
      "stop"  -> TieStop
      _       -> TieNone

note :: [Direction] -> Element -> Note
note ds e = Note {
  noteType = parseNoteType (cval "type" e),
  noteMod  = if chord e then Chord
             else if dot e then Dotted
             else if grace e then Grace
             else ModNone,
  noteDir  = case ds of
               []      -> DirNone
               (d : _) -> d,
  slur     = getSlur e,
  tie      = getTie  e,
  tuplet   = getTuplet e,
  numr     = numRest e }

direction :: Element -> Direction
direction e = case childR ["direction-type", "dynamics"] e of
  Nothing -> DirNone
  Just x  -> case elChildren x of
    []      -> DirNone
    (y : _) -> Dynamic (qName (elName y))

notedir :: [Direction] -> Element -> (Maybe Note, [Direction])
notedir ds e = case qName (elName e) of
  "direction" -> (Nothing, direction e : ds)
  _           -> (Just (note ds e), [])

notedirs :: [Direction] -> [Element] -> [Note]
notedirs ds [] = []
notedirs ds (x : xs) = case notedir ds x of
  (Just n, ds')  -> n : notedirs ds' xs
  (Nothing, ds') -> notedirs ds' xs

-- only handles major keys
key :: Element -> Maybe Key
key e = case cvalR ["attributes", "key", "fifths"] e of
  Nothing -> Nothing
  Just "1" -> Just KeyG
  Just "2" -> Just KeyD
  Just "3" -> Just KeyA
  Just "4" -> Just KeyE
  Just "-1" -> Just KeyF
  Just  _   -> undefined

time :: Element -> Maybe Time
time e = case (cvalR ["attributes", "time", "beats"] e,
               cvalR ["attributes", "time", "beat-type"] e) of
  (Just x, Just y) -> Just (Time x y)
  _                -> Nothing

tempo :: Element -> Maybe Tempo
tempo e = case (cvalR ["direction", "direction-type", "metronome", "beat-unit"] e,
                cvalR ["direction", "direction-type", "metronome", "per-minute"] e) of
  (Just x, Just y) -> Just (Tempo (parseNoteType  x) y)
  _                -> Nothing

measure :: Element -> Measure
measure e = Measure {
  mNumber = attr "number" e,
  mEmpty  = 0,
  mKey    = key e,
  mTime   = time e,
  mTempo  = tempo e,
  mNotes  = notedirs [] (children2 "note" "direction" e) }

measures :: Element -> [Measure]
measures e = case (child "part" e) of
  Nothing -> []
  Just x  -> map measure (children "measure" x)

title :: Element -> String
title e = case (cvalR ["work", "work-title"] e) of
  Nothing -> ""
  Just x  -> x

composer :: Element -> String
composer e = case (cvalR ["identification", "creator"] e) of
  Nothing -> ""
  Just x  -> x

-- Assumes one part per file
instrument :: Element -> String
instrument e = case (cvalR ["part-list", "score-part", "part-name"] e) of
  Nothing -> ""
  Just x  -> x

score :: [Content] -> Score
score xs =
  let top = onlyElems xs !! 1
  in Score {
    sTitle      = title top,
    sComposer   = composer top,
    sInstrument = instrument top,
    sMeasures   = measures top }

xmlToScore :: String -> Score
xmlToScore = score . parseXML
