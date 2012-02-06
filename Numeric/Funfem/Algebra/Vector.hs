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

type Vector = V.Vector Double

dotProd :: Vector -> Vector -> Double
dotProd v w = V.sum $ V.zipWith (*) v w

(.*) :: Vector -> Vector -> Double
v .* w = V.sum $ V.zipWith (*) v w

norm :: Vector -> Double
norm v = sqrt $ v .* v

instance Num Vector where
  negate = V.map negate 
  abs = V.map abs
  fromInteger = undefined
  signum = V.map signum 
  (+) = V.zipWith (+)
  (*) = V.zipWith (*)

