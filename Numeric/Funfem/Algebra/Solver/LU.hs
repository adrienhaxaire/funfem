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
import Data.List (replicate)

import Numeric.Funfem.Algebra.Vector
import Numeric.Funfem.Algebra.Matrix


luSolve = undefined

-- solve Ly = b
--findY :: Matrix -> Vector -> Vector -> Vector
--findY ls ys bs = if V.null us then xs else findX (V.init us) (V.cons x xs) ys



-- solve Ux = y
findX :: Matrix -> Vector -> Vector -> Vector
findX us xs ys = if V.null us then xs else findX (V.init us) (V.cons x xs) ys
  where 
    u = V.last us 
    y = V.drop (V.length us - 1) ys
    x = (V.head y - dotProd (V.tail u) xs) / V.head u

-- values are ok, but ordering is not
luFact :: Matrix -> (Matrix, Matrix)
luFact m | V.length m < 2 = (V.empty, V.empty)
         | otherwise = (reorder lower V.empty, upper)
  where
    (lower, upper) = luFact' (V.empty, V.empty) m

luFact' :: (Matrix, Matrix) -> Matrix -> (Matrix, Matrix)
luFact' (lower, upper) m | V.null m        = (V.empty, V.empty) 
                         | V.length m == 1 = (lower V.++ matrix [[1.0]], upper V.++ matrix [[u11]])
                         | otherwise       = luFact' (lower V.++ l, upper V.++ u) minorLU
  where
    minorLU = minor m - vecProd matL21 matU12    
    u11 = V.head $ V.head m
    matU12 = V.tail $ V.head m  
    matL21 = V.map (/ u11) $ V.tail $ headColumn m
    uh = V.cons u11 matU12
    u = V.cons uh V.empty
    l = V.fromList [vector [1.0] V.++ matL21]

reorder :: Matrix -> Matrix -> Matrix
reorder ls rs = if V.length ls == 1 then V.cons r rs else reorder ls' (V.cons r rs)
  where
    r = V.map V.last ls
    ls' = V.init $ V.map V.init ls



a :: Matrix
a = matrix [[8.0, 2.0, 9.0], [4.0, 9.0, 4.0], [6.0, 7.0, 9.0]]
