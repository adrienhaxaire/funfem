---------------------------------------------------------------------------------- 
-- |
-- Module : Algorithms
-- Copyright : (c) Adrien Haxaire 2011
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Algorithms where

newtype Vector v = Vector [v] deriving (Eq, Show)

instance Num v => Num (Vector v) where
  Vector [a,b] + Vector [a',b'] = Vector [a+a',b+b']
  negate (Vector [a,b]) = Vector [-a,-b]
  Vector [a,b] * Vector [a',b'] = Vector [a*a',b*b']
  fromInteger n = Vector [fromInteger n]

{-
dot :: Vector -> Vector -> Double
dot v1 v2 = foldl (+) v1*v2
dot [] [] = 0.0
-}