---------------------------------------------------------------------------------- 
-- |
-- Module : Numeric.Funfem.Algebra.Tensor
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
-- This modules implements the Tensor data type to 
-- deal with sparse matrices and vectors.
--
------------------------------------------------------------------------

module Numeric.Funfem.Algebra.Tensor (
                                      Tensor
                                      , Index
                                      , Dimension
                                      -- * Constructors
                                      , fromList
                                      , vector
                                      , matrix
                                      -- * Information
                                      , rows
                                      , cols
                                      , dim
                                      , isSquare
                                      -- * Slices
                                      , row
                                      , col
                                      , diag
                                      -- * Operations
                                      , (+!)
                                      , (-!)
                                      , (*!)
                                      , (/!)
                                      , (*.)
                                      , transpose
                                      , minor
                                      , norm
                                      , merge
                                      , mergeWith
                                     ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Control.Arrow (first, second)

type Index = (Int,Int)
type Dimension = (Int,Int)

data Tensor a = Empty | Matrix (M.Map Index a) Dimension
                deriving (Eq, Ord, Show)
{-
instance Show a => Show (Tensor a) where
    show Empty = "The Tensor is empty."
    show (Matrix m (r,c)) = show r ++ "x" ++ show c ++ " Tensor [" ++ sh [1..r] ++ " ]"
        where
          sh (i:is) = let rowi = M.filterWithKey (\k _ -> fst k == i) m
                      in "\n" ++ init (drop 10 $ show rowi) ++ sh is
          sh [] = ""
-}

                                           
instance Functor Tensor where
    fmap _ Empty = Empty
    fmap f (Matrix m rc) = Matrix (M.map f m) rc

-- | Builds a 'Tensor' from an association list.
fromList :: Ord a => [(Index, a)] -> Tensor a
fromList l = Matrix (M.fromList l) d
    where 
      d = (maximum $ map fst ds, maximum $ map snd ds)
      ds = map fst l

-- | Builds a 'Tensor' from a list of lists. The resulting 'Tensor' is
-- therefore not sparse. To build a sparse 'Tensor', use the
-- 'fromList' function.
matrix :: [[a]] -> Tensor a
matrix [] = Empty
matrix ls = Matrix m (is, js)
    where
      m = M.fromList $ zip indexes xs
      indexes = [(i,j) | i <- [1..is], j <- [1..js]]
      xs = concat ls
      is = length ls 
      js = maximum $ map length ls     

-- | Builds a 'Tensor' from a list. The resulting 'Tensor' is
-- therefore not sparse. To build a sparse 'Tensor', use the
-- 'fromList' function.
vector :: [a] -> Tensor a
vector [] = Empty
vector l = Matrix m' (len,1) 
    where
      m' = M.fromList $ zip [(i,1) | i <- [1..len]] l 
      len = length l

-- | Yields the association list corresponding to the 'Tensor'.
toList :: Tensor a -> [(Index, a)]
toList (Matrix m _) = M.toList m
toList Empty = []

-- | Yields the dimension of a 'Tensor'.       
dim :: Tensor a -> Dimension
dim (Matrix _ rc) = rc
dim Empty = (0,0)

-- | Checks if the 'Tensor' is square.
isSquare :: Tensor a -> Bool
isSquare Empty = False
isSquare m = uncurry (==) $ dim m

-- | Yields the number of rows in a 'Tensor'.
rows :: Tensor a -> Int
rows = fst . dim

-- | Yields the number of columns in a 'Tensor'.
cols :: Tensor a -> Int
cols = snd . dim

-- | Extracts a row from a 'Tensor'. The resulting 'Tensor' has the
-- 'dim'ension of a 'transpose'd 'Tensor', i.e. (1,cols).
row :: Int -> Tensor a -> Tensor a
row n (Matrix m (r,c)) = if n > r then Empty else Matrix m' (1,c)
    where
      m' = M.fromList $ map (first $ first (const 1)) $ M.toList mf
      mf = M.filterWithKey (\k _ -> fst k == n) m
row _ Empty = Empty

-- | Extracts a column from a 'Tensor'. The resulting 'Tensor' has the
-- 'dim'ension of a 'Tensor', i.e. (rows,1).
col :: Int -> Tensor a -> Tensor a
col n (Matrix m (r,c)) = if n > c then Empty else Matrix m' (r,1)
    where
      m' = M.fromList $ map (first $ second (const 1)) $ M.toList mf
      mf = M.filterWithKey (\k _ -> snd k == n) m
col _ Empty = Empty

-- | Extracts the diagonal of a 'Tensor' if it is square. The result is a
-- square 'Tensor' with values only on the diagonal.
diag :: Tensor a -> Tensor a
diag (Matrix m (r,c)) | r /= c    = Empty
                      | otherwise = Matrix (go [1..r] M.empty) (r,c)
                      where
                        go [] m' = m'
                        go (x:xs) m' = case (M.lookup (x,x) m) of
                                         Nothing -> go xs m'
                                         Just v -> go xs (M.insert (x,x) v m')
diag _ = Empty

at :: (Num a) => Tensor a -> Index -> a
at (Matrix m (r,c)) (i,j) | i > r     = 0
                          | j > c     = 0
                          | otherwise = fromMaybe 0 $ M.lookup (i,j) m
at Empty _ = 0

-- | Transposes a 'Tensor'. The conflicting name can be avoided with a 
-- named import, not necessarily qualified:
--
-- > import Numeric.Funfem.Algebra.Tensor as T
transpose :: Tensor a -> Tensor a 
transpose (Matrix m (r,c)) = Matrix m' (c,r)
    where
      m' = M.fromList $ map (first swap) $ M.toList m 
transpose Empty = Empty

-- | The 'minor' function returns a 'Tensor' without the specified row and
-- column.
minor :: Index -> Tensor a -> Tensor a
minor (i,j) (Matrix m (r,c)) | i > r     = Empty
                             | j > c     = Empty 
                             | otherwise = Matrix m' (r-1,c-1)
    where
      m' = M.filterWithKey (\k _ -> snd k /= j) $ 
           M.filterWithKey (\k _ -> fst k /= i) m
minor _ _ = Empty

infixl 7 *.
-- | Dot product between two 'Tensor's. To keep the function total, a value of 
-- zero is returned in case of uncompatible dimensions, as it is unlikely
-- that a result of zero can be useful.
(*.) :: Num a => Tensor a -> Tensor a -> a
(*.) (Matrix m (1,c)) (Matrix n (r,1)) = if c /= r then 0 else x
    where
      x = M.foldl' (+) 0 $ M.unionWith (*) m nt
      nt = M.fromList $ map (first swap) $ M.toList n
(*.) m@(Matrix _ (rm,1)) n@(Matrix _ (rn,1)) = if rm /= rn then 0 
                                              else transpose m *. n
(*.) _ _ = 0

-- | Calculates the 'norm' of a 'Tensor'. A value of zero is returned if the
-- dimensions of the 'Tensor' are not vector-like (i.e. not one column).
norm :: (Floating a, Num a) => Tensor a -> a
norm v = sqrt $ v *. v

vecProd :: Num a => Tensor a -> Tensor a -> Tensor a
vecProd m@(Matrix _ (r,1)) n@(Matrix _ (1,c)) = Matrix (M.fromList alist) (r,c)
    where
      alist = [((i,j), (m `at` (i,1)) * (n `at` (1,j))) | i <- [1..r], j <- [1..c]]
vecProd _ _ = Empty

add :: Num a => Tensor a -> Tensor a -> Tensor a
add (Matrix m (rm,cm)) (Matrix n (rn,cn)) 
    | rm /= rn  = Empty
    | cm /= cn  = Empty
    | otherwise = Matrix (M.unionWith (+) m n) (rm,cm)
add _ _ = Empty

mult :: (Eq a, Num a) => Tensor a -> Tensor a -> Tensor a
mult m@(Matrix _ (1,c)) n@(Matrix _ (r,1)) 
    | c /= r   = Empty 
    |otherwise = Matrix (M.singleton (1,1) (m *. n)) (1,1)
mult m@(Matrix _ (r,1)) n@(Matrix _ (1,c))
    | r /= c    = Empty 
    | otherwise = vecProd m n
mult m@(Matrix _ (rm,cm)) n@(Matrix _ (rn,cn))
    | cm /= rn  = Empty 
    | otherwise = Matrix m' (rm, cn)
    where 
      m' = M.filter (/= 0) $ M.fromList alist
      alist = [((i,j), row i m *. col j n) | i <- [1..rm], j <- [1..cn]]
mult _ _ = Empty

instance (Eq a, Num a) => Num (Tensor a) where
  negate = fmap negate
  abs = fmap abs
  fromInteger = undefined
  signum = fmap signum
  (+) = add
  (*) = mult

infixl 7 *!
-- | Elementwise multiplication
(*!) :: Num a => a -> Tensor a -> Tensor a
(*!) x t = fmap (*x) t

infixl 7 /!
-- | Elementwise division.
(/!) :: (Fractional a, Num a) => a -> Tensor a -> Tensor a
(/!) x t = fmap (/x) t

infixl 6 +!
-- | Elementwise addition.
(+!) :: Num a => a -> Tensor a -> Tensor a
(+!) x t = fmap (+x) t

infixl 6 -!
-- | Elementwise substraction.
(-!) :: Num a => a -> Tensor a -> Tensor a
(-!) x t = fmap (\y -> y-x) t

-- | The 'merge' function allows the superposition of two 'Tensors' with 
-- different 'dim'ensions. It is left biased, i.e. the terms in the left 
-- 'Tensor' prevail, unless it is empty.
merge :: Tensor a -> Tensor a -> Tensor a
merge (Matrix m (rm,cm)) (Matrix n (rn,cn)) = Matrix (m `M.union` n) (max rm rn, max cm cn)
merge Empty m = m
merge m Empty = m

-- | The 'mergeWith' functions applies the given function when merging two 
-- 'Tensor's. It is obviously useful when building the global stiffness
-- matrix for example.
mergeWith :: (a -> a -> a) -> Tensor a -> Tensor a -> Tensor a
mergeWith f (Matrix m (rm,cm)) (Matrix n (rn,cn)) = Matrix (M.unionWith f m n) (max rm rn, max cm cn)
mergeWith _ Empty m = m
mergeWith _ m Empty = m



-- Symmetric Positive Definite matrix iff:
--    symmetric
--    all the diagonal entries are positive
--    each diagonal entry is greater than the sum of the absolute values of all other entries in the corresponding row/column.
spd :: (Eq a, Ord a, Num a) => Tensor a -> Bool
spd m = symmetric m && positive m && definite m

symmetric :: Eq a => Tensor a -> Bool
symmetric m = transpose m == m

-- are all the diagonal entries positive ?
positive :: (Ord a, Num a) => Tensor a -> Bool
positive = all (> 0) . diags

diags :: Tensor a -> [a]
diags = map (\x -> snd x) . toList . diag                 

-- is each diagonal entry greater than the sum of the absolute values of all other entries in the corresponding row/column ?
definite :: (Ord a, Num a) => Tensor a -> Bool
definite m = let ds = diags m
                 sums = [sum $ map (\x -> abs (snd x)) $ filter (\(k,_) -> fst k /= snd k) $ toList (col c m) | c <- [1..cols m]]
             in and $ zipWith (>) ds sums

-- pb with diagonal to remove before doing the sum

*Numeric.Funfem.Algebra.Tensor> let m = matrix [[1,2],[3,4]]
*Numeric.Funfem.Algebra.Tensor> definite m
False
*Numeric.Funfem.Algebra.Tensor> :i filter
filter :: (a -> Bool) -> [a] -> [a] 	-- Defined in `GHC.List'
*Numeric.Funfem.Algebra.Tensor> filter (>0) [-1,1]
[1]
*Numeric.Funfem.Algebra.Tensor> toList m
[((1,1),1),((1,2),2),((2,1),3),((2,2),4)]
*Numeric.Funfem.Algebra.Tensor> filter (\(k,_) -> fst k /= snd k) $ toList m
[((1,2),2),((2,1),3)]
*Numeric.Funfem.Algebra.Tensor> filter (\(k,_) -> fst k /= snd k) $ toList $ col 1 m
[((2,1),3)]
*Numeric.Funfem.Algebra.Tensor> map (\x -> abs (snd x)) $ filter (\(k,_) -> fst k /= snd k) $ toList $ col 1 m
[3]
*Numeric.Funfem.Algebra.Tensor> 