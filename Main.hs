module Main where

import System.Environment

import Gmsh

main :: IO ()
main = do
  text <- readFile "test.msh"
  let (nodes, elements) = importGmsh text
  print elements
  


--  [filename] <- getArgs
--  text <- readFile filename


