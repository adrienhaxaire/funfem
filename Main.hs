module Main where

import System.Environment

import Gmsh

main :: IO ()
main = do
  [filename] <- getArgs
  text <- readFile filename
  let (nodes, elements) = importGmsh text
  print elements

