module Main where

import Numeric.Funfem.Vector
import Numeric.Funfem.Solver


main :: IO()
main = do
  let m = Matrix [Vector [1,2], Vector [3,4]]
  let x = Vector [0,0]
  let v = Vector [3,80]
  let r = cg m x v
  print r  