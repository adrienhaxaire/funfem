---------------------------------------------------------------------------------- 
-- |
-- Module : Matrix
-- Copyright : (c) Adrien Haxaire 2011
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Matrix where

import Data.List as L

import Numeric.Funfem.Vector

data Matrix = Matrix [Vector]
            deriving (Eq, Ord, Show)

instance Num Matrix where
  negate (Matrix a) = Matrix [-a' | a' <- a]
  abs (Matrix a) = Matrix [abs a' | a' <- a]
  fromInteger n = undefined
  signum (Matrix a) = Matrix [signum a' | a' <- a]
  Matrix a + Matrix b = Matrix (zipWith (+) a b)
  Matrix a * Matrix b = Matrix [fromList [a' .* b' | b' <- bt] | a' <- a]
    where
      bt = L.map (fromList) (L.transpose [fromVector v | v <- b])

fromVectors :: [Vector] -> Matrix
fromVectors v = Matrix v

fromMatrix :: Matrix -> [Vector]
fromMatrix (Matrix m) = m

fromMatrix' :: Matrix -> [[Double]]
fromMatrix' m = [fromVector v | v <- fromMatrix m]

new :: Int -> Double -> Matrix
new n d = fromVectors $ take n (repeat (Numeric.Funfem.Vector.new n d))

dim :: Matrix -> (Int, Int)
dim m = (size $ L.head $ fromMatrix m, length $ fromMatrix m) 

transpose :: Matrix -> Matrix
transpose m = fromVectors [fromList l | l <- L.transpose $ fromMatrix' m]  

tensor_product :: Vector -> Vector -> Matrix
tensor_product vs ws = fromVectors [Numeric.Funfem.Vector.map (*v) ws | v <- fromVector vs]

multMV :: Matrix -> Vector -> Vector
{-# INLINE multMV #-}
multMV m v = fromList $ L.map (.* v) (fromMatrix m) 

-- | Safe matrix multiplication
multMM :: Matrix -> Matrix -> Maybe Matrix
multMM a b = if (size $ L.head $ fromMatrix a) /= (length $ fromMatrix b)
             then Nothing
             else Just mult 
  where
    mult = fromVectors [fromList [a' .* b' | b' <- fromMatrix $ transpose' b] | a' <- fromMatrix a]
    transpose' = Numeric.Funfem.Matrix.transpose
    
-- | Scalar to Matrix multiplication
multSM :: Double -> Matrix -> Matrix
{-# INLINE multSM #-}
multSM x m = fromVectors [vmap (*x) v | v <- fromMatrix m]

-- | Returns a matrix without row and column 
butRowColumn :: Int -> Int -> Matrix -> Matrix
butRowColumn r c m = fromVectors $ butRow r $ butColumn c $ fromMatrix m
  where
    butColumn c' (v:vs) = [butSlice c' c' v] L.++ butColumn c' vs
    butColumn _ [] = []    
    butRow r' m' = pre L.++ post
      where 
        pre = fst splat
        post = L.tail $ (snd splat)
        splat = splitAt (r'-1) m'        
  
isSquare :: Matrix -> Bool
isSquare m = let (rows,cols) = dim m in rows == cols
  
-- | Safe determinant, checks for square matrix
maybeDet :: Matrix -> Maybe Double
maybeDet m = if (isSquare m) then Just (det m) else Nothing

det :: Matrix -> Double
det (Matrix []) = 0.0
det (Matrix [Vector [a]]) = a
det m = if size fstRow == 2 then det2x2 m else subdets
  where
    fstRow = L.head $ fromMatrix m
    subdets = L.sum $ L.zipWith (*) cofs dets
    cofs = [if even i then (fstRow ! i) else -(fstRow ! i) | i <- [0..(n-1)]]
    dets = [det (butRowColumn 1 i m) | i <- [1..n]]
    n = size fstRow

det2x2 :: Matrix -> Double
det2x2 m = det2x2' (L.head vs) (L.last vs)
  where
    vs = fromMatrix m
    det2x2' v w = head' v * last' w - last' v * head' w
    head' = Numeric.Funfem.Vector.head
    last' = Numeric.Funfem.Vector.last

-- | Extracts given row
row :: Int -> Matrix -> Vector    
row r m = (fromMatrix m) L.!! (r-1)   
    
-- | Extracts given column          
col :: Int -> Matrix -> Vector
col c m = row c $ Numeric.Funfem.Matrix.transpose m   

-- | Extracts submatrix formed by a number of rows and columns from a
-- matrix at specified position
extract :: Int -> Int -> Matrix -> (Int, Int) -> Matrix
extract r c m (i,j) = fromVectors $ cols $ rows
  where
    rows = [row r' m | r' <- [i..(i+r-1)]]
    cols = L.map (slice j (j+c-1))
    
        
-- | Inserts (adds) first matrix into second one at given position (top left
-- corner). NB: inserting a bigger matrix in a smaller one will extend
-- it!
--insert :: Matrix -> Matrix -> (Int, Int) -> Matrix
--insert f s (i,j) = 
--  where
--    (rows,cols) = dim f
--    extract (rows-i) (cols-j) s (i,j)