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
  abs (Vector a) = Vector [abs a' | a' <- a]
  fromInteger n = Vector [fromInteger n]
  signum (Vector a) = Vector [signum a' | a' <- a]

fromVector :: Vector -> [Double]
fromVector (Vector v) = v

fromList :: [Double] -> Vector 
fromList v = Vector v

head :: Vector -> Double
head = L.head . fromVector

tail :: Vector -> [Double]
tail = L.tail . fromVector

last :: Vector -> Double
last = L.last . fromVector

init :: Vector -> [Double]
init = L.init . fromVector

slice :: Int -> Int -> Vector -> Vector
slice b e v = fromList $ take (e-b+1) $ drop (b-1) $ fromVector v

butSlice :: Int -> Int -> Vector -> Vector
butSlice b e v = fromList $ (pre L.++ post)
  where
    pre = take (b-1) $ fromVector v
    post = drop e $ fromVector v

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
concat v1 v2 = Vector (fromVector v1 L.++ fromVector v2)

-- | Infix vector concatenation
(++) :: Vector -> Vector -> Vector
v1 ++ v2 = Vector (fromVector v1 L.++ fromVector v2)

map :: (Double -> Double) -> Vector -> Vector
map f v = fromList $ L.map f (fromVector v)

-- | Alias for local map, to avoid ambiguity with Data.List.map when fully imported (without the 'as' keyword)
vmap :: (Double -> Double) -> Vector -> Vector
vmap = Numeric.Funfem.Vector.map

norm :: Vector -> Double
{-# INLINE norm #-}
norm v = sqrt (v .* v)

data Matrix = Matrix [Vector]
            deriving (Eq, Ord, Show)

fromVectors :: [Vector] -> Matrix
fromVectors v = Matrix v

fromMatrix :: Matrix -> [Vector]
fromMatrix (Matrix m) = m

fromMatrix' :: Matrix -> [[Double]]
fromMatrix' m = [fromVector v | v <- fromMatrix m]

transpose :: Matrix -> Matrix
transpose m = fromVectors [fromList l | l <- L.transpose $ fromMatrix' m]  

tensor_product :: Vector -> Vector -> Matrix
tensor_product vs ws = fromVectors [Numeric.Funfem.Vector.map (*v) ws | v <- fromVector vs]

multMV :: Matrix -> Vector -> Vector
{-# INLINE multMV #-}
multMV m v = fromList $ L.map (.* v) (fromMatrix m) 

multMM :: Matrix -> Matrix -> Matrix
multMM a b = fromVectors [fromList [a' .* b' | b' <- fromMatrix b] | a' <- fromMatrix a]


--multSM :: Double -> Matrix -> Matrix

-- | Drops row and column
 
-- mdrop m r c = fromVectors $ 
  
  
  
