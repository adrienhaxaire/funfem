{-# LANGUAGE TypeSynonymInstances #-}

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

module Numeric.Funfem.Matrix where

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

transpose :: Matrix -> Matrix
transpose m = V.cons (V.map V.head m) (transpose $ V.map V.tail m)


instance Num Matrix where
  negate = V.map negate 
  abs = V.map abs
  fromInteger n = undefined
  signum = V.map signum 
  (+) = V.zipWith (+)
  (*) = V.zipWith (*)


-- instance Num Matrix where
  



{-
instance Num Matrix where
  negate (Matrix a) = Matrix [-a' | a' <- a]
  abs (Matrix a) = Matrix [abs a' | a' <- a]
  fromInteger = undefined
  signum (Matrix a) = Matrix [signum a' | a' <- a]
  Matrix a + Matrix b = Matrix (zipWith (+) a b)
  Matrix a * Matrix b = Matrix [V.fromList [a' .* b' | b' <- bt] | a' <- a]
    where
      bt = L.map V.fromList (L.transpose [fromVector v | v <- b])

fromVectors :: [V.Vector] -> Matrix
fromVectors = Matrix

fromMatrix :: Matrix -> [V.Vector]
fromMatrix (Matrix m) = m

fromMatrix' :: Matrix -> [[Double]]
fromMatrix' m = [fromVector v | v <- fromMatrix m]

fromLists :: [[Double]] -> Matrix
fromLists lists = fromVectors [V.fromList l | l <- lists]

genMatrix :: Int -> Double -> Matrix
genMatrix n d = fromVectors $ replicate n (V.genVector n d)

dim :: Matrix -> (Int, Int)
dim m = (size $ L.head $ fromMatrix m, length $ fromMatrix m) 

transpose :: Matrix -> Matrix
transpose m = fromVectors [V.fromList l | l <- L.transpose $ fromMatrix' m]  

tensorProduct :: V.Vector -> V.Vector -> Matrix
tensorProduct vs ws = fromVectors [V.map (*v) ws | v <- fromVector vs]

multMV :: Matrix -> V.Vector -> Vector
{-# INLINE multMV #-}
multMV m v = V.fromList $ L.map (.* v) (fromMatrix m) 

-- | Safe matrix multiplication
multMM :: Matrix -> Matrix -> Maybe Matrix
multMM a b = if size (L.head $ fromMatrix a) /= length (fromMatrix b)
             then Nothing
             else Just mult 
  where
    mult = fromVectors [V.fromList [a' .* b' | b' <- fromMatrix $ transpose' b] | a' <- fromMatrix a]
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
        post = L.tail $ snd splat
        splat = splitAt (r'-1) m'        
  
isSquare :: Matrix -> Bool
isSquare m = let (rows,cols) = dim m in rows == cols
  
-- | Safe determinant, checks for square matrix
maybeDet :: Matrix -> Maybe Double
maybeDet m = if isSquare m then Just (det m) else Nothing

det :: Matrix -> Double
det (Matrix []) = 0.0
det (Matrix [Vector [a]]) = a
det m = if size fstRow == 2 then det2x2 m else subdets
  where
    fstRow = L.head $ fromMatrix m
    subdets = L.sum $ L.zipWith (*) cofs dets
    cofs = [if even i then fstRow ! i else -(fstRow ! i) | i <- [0..(n-1)]]
    dets = [det (butRowColumn 1 i m) | i <- [1..n]]
    n = size fstRow

det2x2 :: Matrix -> Double
det2x2 m = det2x2' (L.head vs) (L.last vs)
  where
    vs = fromMatrix m
    det2x2' v w = V.head v * V.last w - V.last v * V.head w

-- | Extracts given row
row :: Int -> Matrix -> Vector    
row r m = fromMatrix m L.!! (r-1)   
    
-- | Extracts given column          
col :: Int -> Matrix -> Vector
col c m = row c $ Numeric.Funfem.Matrix.transpose m   

-- | Extracts submatrix formed by a number of rows and columns from a
-- matrix at specified position
extract :: Int -> Int -> Matrix -> (Int, Int) -> Matrix
extract r c m (i,j) = fromVectors $ cols rows
  where
    rows = [row r' m | r' <- [i..(i+r-1)]]
    cols = L.map (slice j (j+c-1))
    
atIndex :: Matrix -> (Int,Int) -> Double
atIndex m index = L.head . L.head . fromMatrix' $ extract 1 1 m index 

        
-- | Inserts (adds) first matrix into second one at given position (top left
-- corner). NB: inserting a bigger matrix in a smaller one will extend
-- it!
insert :: Matrix -> Matrix -> (Int, Int) -> Matrix
insert f s (i,j) = e
  where
    (rows,cols) = dim f
    e = extract (rows-i) (cols-j) s (i,j)

-}