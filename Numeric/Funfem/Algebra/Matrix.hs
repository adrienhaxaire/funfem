{-# LANGUAGE FlexibleInstances #-}
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

import Data.List (transpose)

import Numeric.Funfem.Algebra.Vector
import qualified Data.Vector as V

type Matrix = V.Vector (V.Vector Double)

matrix :: [[a]] -> V.Vector (V.Vector a)
matrix l = V.fromList $ map vector l

toLists :: V.Vector (V.Vector a) -> [[a]]
toLists m = if V.null m then [] else toList (V.head m) : toLists (V.tail m)

headColumn :: V.Vector (V.Vector a) -> V.Vector a
headColumn = V.map V.head

tailColumns :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
tailColumns = V.map V.tail

headRow :: V.Vector a -> a
headRow = V.head

tailRows :: V.Vector a -> V.Vector a
tailRows = V.tail

nullMatrix :: V.Vector (V.Vector a) -> Bool
nullMatrix = V.null . V.head 

transposeVector :: V.Vector a -> V.Vector (V.Vector a)
transposeVector v = matrix $ transpose [toList v]

transposeMatrix :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
transposeMatrix = matrix . transpose . toLists

multMV :: Num a => V.Vector (V.Vector a) -> V.Vector a -> V.Vector a
multMV m v = V.map (dotProd v) m

multMM :: Num a => V.Vector (V.Vector a) -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
multMM m n = V.map row m 
  where
    row r = V.map (dotProd r) tn
    tn = transposeMatrix n

dim :: V.Vector (V.Vector a) -> (Int, Int)
dim m = (V.length m, V.length $ V.head m)

isSquare :: V.Vector (V.Vector a) -> Bool
isSquare m = let (rows, cols) = dim m in rows == cols

instance Num a => Num (V.Vector (V.Vector a)) where
  negate = V.map negate 
  abs = V.map abs
  fromInteger = undefined
  signum = V.map signum 
  (+) = V.zipWith (+)
  (*) = multMM

vecProd :: Num a => V.Vector a -> V.Vector a -> V.Vector (V.Vector a)
vecProd v w = if V.null v then V.empty else V.cons (V.map (*V.head v) w) (vecProd (V.tail v) w)  

minor :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
minor = V.tail . tailColumns

