module Main where

import System.Environment
import Text.ParserCombinators.Parsec

import Gmsh

main :: IO ()
main = do
  [filename] <- getArgs
  text <- readFile filename
  print $ importGmsh text


