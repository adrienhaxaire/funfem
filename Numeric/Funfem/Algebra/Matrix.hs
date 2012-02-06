{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
---------------------------------------------------------------------------------- 
-- |
-- Module : Matrix
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Algebra.Matrix where

import Numeric.Funfem.Algebra.Vector
import qualified Data.Vector as V

type Matrix = V.Vector Vector 

headColumn :: Matrix -> Vector
headColumn = V.map V.head

tailColumns :: Matrix -> Matrix
tailColumns = V.map V.tail

headRow :: Vector -> Double
headRow = V.head

tailRows :: Vector -> Vector
tailRows = V.tail

nullMatrix :: Matrix -> Bool
nullMatrix = V.null . V.head 

transpose :: Matrix -> Matrix
transpose m | nullMatrix m = V.empty
            | otherwise    = V.fromList [headColumn m] V.++ transpose (tailColumns m)

multMV :: Matrix -> Vector -> Vector
multMV m v = V.map (dotProd v) m

multMM :: Matrix -> Matrix -> Matrix
multMM m n = V.map row m 
  where
    row r = V.map (dotProd r) tn
    tn = transpose n

dim :: Matrix -> (Int, Int)
dim m = (V.length m,V.length $ V.head m)

isSquare :: Matrix -> Bool
isSquare m = let (rows, cols) = dim m in rows == cols

instance Num Matrix where
  negate = V.map negate 
  abs = V.map abs
  fromInteger = undefined
  signum = V.map signum 
  (+) = V.zipWith (+)
  (*) = multMM

