---------------------------------------------------------------------------------- 
-- |
-- Module : LU
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Algebra.Solver.LU where


import qualified Data.Vector as V

import Numeric.Funfem.Algebra.Vector
import Numeric.Funfem.Algebra.Matrix

luSolve = undefined


a = matrix [[8.0, 2.0, 9.0], [4.0, 9.0, 4.0], [6.0, 7.0, 9.0]]

-- fst row of U, fst column of L
luFact :: Matrix -> (Matrix, Matrix)
luFact m | V.length m < 2 = (V.empty, V.empty)
         | otherwise = (reorder lower, upper)
  where
    (lower, upper) = luFact' (V.empty, V.empty) m
             
{-
-- fromList [fromList [1.0,0.5,0.75],fromList [1.0,0.6875],fromList [1.0]]
reorder :: Matrix -> Matrix
reorder m = go m V.empty (V.length m)
  where
    go x r n = if n == 0 then V.empty else go x r' n-1
      where
        r' = r 
-}               
reorder = id             




luFact' :: (Matrix, Matrix) -> Matrix -> (Matrix, Matrix)
luFact' (lower, upper) m | V.null m        = (V.empty, V.empty) 
                         | V.length m == 1 = (lower V.++ matrix [[1.0]], upper V.++ matrix [[u11 m]])
                         | otherwise       = luFact' (lower V.++ l m, upper V.++ u m ) (minorLU m)

u11 :: Matrix -> Double
u11 = V.head . V.head

matU12 :: Matrix -> Vector
matU12 = V.tail . V.head   

matL21 :: Matrix -> Vector
matL21 m = V.map (/(u11 m)) $ V.tail $ headColumn m

headU :: Matrix -> Vector
headU m = V.cons (u11 m) (matU12 m)

u :: Matrix -> Matrix
u m = V.cons (headU m) V.empty

l :: Matrix -> Matrix
l m = V.fromList [vector [1.0] V.++ matL21 m]

minorLU :: Matrix -> Matrix
minorLU m = minor m - vecProd (matL21 m) (matU12 m)


