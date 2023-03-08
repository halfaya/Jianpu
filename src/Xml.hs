{-# LANGUAGE
  ScopedTypeVariables,
  NamedFieldPuns,
  RecordWildCards,
  OverloadedRecordDot
#-}

module Xml where

import Data.List (transpose, intercalate, intersperse, groupBy)
import Data.List.Split (chunksOf)
import Text.XML.Light
import Text.XML.Light.Output

import Music

attr :: String -> Element -> String
attr s e = case findAttrBy (\q -> qName q == s) e of
  Just x  -> x
  Nothing -> "NOTFOUND"

children :: String -> Element -> [Element]
children s = filterChildrenName (\q -> qName q == s)

child :: String -> Element -> Maybe Element
child s e = case (children s e) of
  []      -> Nothing
  (x : _) -> Just x

cval :: String -> Element -> String
cval s e = case (child s e) of
  Nothing -> ""
  Just x  -> strContent x

type MNum     = String

type Measure   = (MNum, [Note])

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
  _                     -> undefined
  
note :: Element -> Note
note e = Note {noteType = parseNoteType (cval "type" e),
               noteMod = if chord e then Chord
                         else if dot e then Dotted
                         else if grace e then GraceStart
                         else ModNone,
               slur = getSlur e,
               tuplet = getTuplet e,
               numr = numRest e}

measure :: Element -> Measure
measure e = (attr "number" e , map note (children "note" e))

measures :: Element -> [Measure]
measures e = map measure (children "measure" e)

part :: [Content] -> Element
part xs = case (child "part" (onlyElems xs !! 1)) of
  Nothing -> undefined
  Just e  -> e

showMeasure :: Measure -> String
showMeasure (_,ns) = intercalate "" (map showNote ns)

showMeasures :: [Measure] -> String
showMeasures ms = intercalate "\n" (map showMeasure ms)

xmlToMeasures :: String -> [Measure]
xmlToMeasures = measures . part . parseXML
