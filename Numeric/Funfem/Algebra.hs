{-# LANGUAGE TypeSynonymInstances #-}

---------------------------------------------------------------------------------- 
-- |
-- Module : Algebra
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Algebra where

import qualified Data.Vector as V

type Vector = V.Vector Double

instance Num Vector where
  negate = V.map negate 
  abs = V.map abs
  fromInteger n = undefined
  signum = V.map signum 
  (+) = V.zipWith (+)
  (*) = V.zipWith (*)

dotProd :: Vector -> Vector -> Double
dotProd v w = V.sum $ V.zipWith (*) v w

(.*) :: Vector -> Vector -> Double
v .* w = V.sum $ V.zipWith (*) v w

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
            | otherwise    = V.fromList [headColumn m] V.++ transpose $ tailColumns m

multMV :: Matrix -> Vector -> Vector
multMV m v = V.map (dotProd v) m

multMM :: Matrix -> Matrix -> Matrix
multMM m n = V.map row m 
  where
    row r = V.map (dotProd r) tn
    tn = transpose n

instance Num Matrix where
  negate = V.map negate 
  abs = V.map abs
  fromInteger n = undefined
  signum = V.map signum 
  (+) = V.zipWith (+)
  (*) = multMM

dim :: Matrix -> (Int, Int)
dim m = (V.length m,V.length $ V.head m)

isSquare :: Matrix -> Bool
isSquare m = let (rows, cols) = dim m in rows == cols
