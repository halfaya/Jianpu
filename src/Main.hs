{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment (getArgs)

import Xml

test = run "/Users/leo/Music/XML/Erhu2.xml" (Just "/Users/leo/Music/XML/in.jianpu")

run :: String -> Maybe String -> IO ()
run file out = do
  s <- readFile file
  let ms   = xmlToMeasures s
  case out of
    Nothing      -> putStrLn $ showMeasures ms
    Just outFile -> writeFile outFile (showMeasures ms ++ "\n")

main :: IO ()
main = do
  args <- getArgs
  run (head args) Nothing
