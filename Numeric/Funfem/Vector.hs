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

data Vector = Vector [Double] 
            deriving (Eq, Ord, Show)

instance Num Vector where
  Vector a + Vector b = Vector [a' + b' | a' <- a, b' <- b]
  Vector a * Vector b = Vector [a' * b' | a' <- a, b' <- b]
  negate (Vector [a,b]) = Vector [-a,-b]
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
dot_product v1 v2 = foldl (+) 0 $ fromVector (v1*v2)

-- | Infix dot product 
(.*) :: Vector -> Vector -> Double
{-# INLINE (.*) #-}
v1 .* v2 = foldl (+) 0 $ fromVector (v1*v2)

concat :: Vector -> Vector -> Vector 
{-# INLINE concat #-}
concat v1 v2 = Vector (fromVector v1 Prelude.++ fromVector v2)

-- | Infix vector concatenation
(++) :: Vector -> Vector -> Vector
{-# INLINE (++) #-}
v1 ++ v2 = Vector (fromVector v1 Prelude.++ fromVector v2)


data Matrix = Matrix [Vector]
            deriving (Eq, Ord, Show)

fromMatrix :: Matrix -> [Vector]
fromMatrix (Matrix m) = m

fromVectors :: [Vector] -> Matrix
fromVectors v = Matrix v

dot :: Matrix -> Vector -> Vector
{-# INLINE dot #-}
dot m v = fromList $ dot' (fromMatrix m) v 
  where
    dot' (w:ws) v' = [w .* v'] Prelude.++ dot' ws v'
    dot' [] _ = []  
