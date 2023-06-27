{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment (getArgs)

import Music
import Xml

prefix :: String
prefix = "/Users/leo/Music/SCO/Yao Tribe/"

test :: String -> IO ()
test x = run (prefix ++ x ++ ".musicxml") (Just (prefix ++ x ++ ".jianpu"))

run :: String -> Maybe String -> IO ()
run file out = do
  s <- readFile file
  let score = xmlToScore s
  case out of
    Nothing      -> putStrLn $ showScore score
    Just outFile -> writeFile outFile (showScore score ++ "\n")

main :: IO ()
main = do
  args <- getArgs
  case args of
    (x : y : _) -> run x (Just y)
    (x : _)     -> run x Nothing
    _           -> putStrLn "Usage: Jianput infile [outfile]"
