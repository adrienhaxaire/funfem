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

import Data.List as L

data Vector = Vector [Double] 
            deriving (Eq, Ord, Show)

instance Num Vector where
  Vector a + Vector b = Vector (zipWith (+) a b)
  Vector a * Vector b = Vector (zipWith (*) a b)
  negate (Vector a) = Vector [-a' | a' <- a]
  fromInteger n = Vector [fromInteger n]

fromVector :: Vector -> [Double]
fromVector (Vector v) = v

fromList :: [Double] -> Vector 
fromList v = Vector v

size :: Vector -> Int
{-# INLINE size #-}
size = length . fromVector

dot_product :: Vector -> Vector -> Double
{-# INLINE dot_product #-}
dot_product v1 v2 = L.foldl' (+) 0 $ fromVector (v1*v2)

-- | Infix dot product 
(.*) :: Vector -> Vector -> Double
{-# INLINE (.*) #-}
v1 .* v2 = L.foldl' (+) 0 $ fromVector (v1*v2)

concat :: Vector -> Vector -> Vector 
{-# INLINE concat #-}
concat v1 v2 = Vector (fromVector v1 Prelude.++ fromVector v2)

-- | Infix vector concatenation
(++) :: Vector -> Vector -> Vector
{-# INLINE (++) #-}
v1 ++ v2 = Vector (fromVector v1 Prelude.++ fromVector v2)

map :: (Double -> Double) -> Vector -> Vector
{-# INLINE map #-}
map f v = fromList $ Prelude.map f (fromVector v)

-- | Alias for local map, to avoid ambiguity with Prelude.map when fully imported (without the 'as' keyword)
vmap :: (Double -> Double) -> Vector -> Vector
vmap = Numeric.Funfem.Vector.map


norm :: Vector -> Double
{-# INLINE norm #-}
norm v = sqrt (v .* v)

data Matrix = Matrix [Vector]
            deriving (Eq, Ord, Show)

fromMatrix :: Matrix -> [Vector]
fromMatrix (Matrix m) = m

fromVectors :: [Vector] -> Matrix
fromVectors v = Matrix v

dot :: Matrix -> Vector -> Vector
{-# INLINE dot #-}
dot m v = fromList $ Prelude.map (.* v) (fromMatrix m) 
