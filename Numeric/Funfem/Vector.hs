---------------------------------------------------------------------------------- 
-- |
-- Module : Vector
-- Copyright : (c) Adrien Haxaire 2011
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Vector where

newtype Vector v = Vector [v] deriving (Eq, Show)

instance Num v => Num (Vector v) where
  
  Vector [a,b] + Vector [a',b'] = Vector [a+a',b+b']
  Vector [] + Vector [a,b] = Vector [a,b]
  Vector [a,b] + Vector [] = Vector [a,b]
  Vector [] + Vector [] = Vector []

  Vector [a,b] * Vector [a',b'] = Vector [a*a',b*b']
  Vector [] * Vector [_] = Vector []
  Vector [_] * Vector [] = Vector []
  Vector [] * Vector [] = Vector []

  negate (Vector [a,b]) = Vector [-a,-b]
    
  fromInteger n = Vector [fromInteger n]


fromVector :: Vector v -> [v]
fromVector (Vector v) = v

fromList :: [v] -> Vector v
fromList v = Vector v

dot_product :: Num v => Vector v -> Vector v -> v
dot_product v1 v2 = foldl (+) 0 $ fromVector (v1*v2)

-- | Infix dot product 
(.*) :: Num v => Vector v -> Vector v -> v
v1 .* v2 = foldl (+) 0 $ fromVector (v1*v2)

concat :: Vector v -> Vector v -> Vector v
concat v1 v2 = Vector (fromVector v1 Prelude.++ fromVector v2)

-- | Infix vector concatenation
(++) :: Vector v -> Vector v -> Vector v
v1 ++ v2 = Vector (fromVector v1 Prelude.++ fromVector v2)