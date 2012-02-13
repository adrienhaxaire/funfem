{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
---------------------------------------------------------------------------------- 
-- |
-- Module : Vector
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Algebra.Vector where

import qualified Data.Vector as V

vector :: [a] -> V.Vector a
vector = V.fromList

toList :: V.Vector  a-> [a]
toList v = if V.null v then [] else V.head v : toList (V.tail v)

dotProd :: Num a => V.Vector a -> V.Vector a -> a
dotProd v w = V.sum $ V.zipWith (*) v w

-- | Infix dot product
(.*) :: Num a => V.Vector a -> V.Vector a -> a
v .* w = V.sum $ V.zipWith (*) v w

norm :: Floating a => V.Vector a -> a
norm v = sqrt $ v .* v

instance Num a => Num (V.Vector a) where
  negate = V.map negate 
  abs = V.map abs
  fromInteger = undefined
  signum = V.map signum 
  (+) = V.zipWith (+)
  (*) = V.zipWith (*)
