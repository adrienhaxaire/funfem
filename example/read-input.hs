module Main where

import Numeric.Funfem

main :: IO()
main = do
  f <- readFile "read-input.json"
  let i = inputFromString f
  let elements = elementsFromInput i    
  print elements
