---------------------------------------------------------------------------------- 
-- |
-- Module : Numeric.Funfem.Algebra.Tensor
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@haxaire.org>
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
                                      , empty
                                      , fromList
                                      , vector
                                      , matrix
                                      -- * Information
                                      , rows
                                      , cols
                                      , dim
                                      , isSquare
                                      , symmetric
                                      , spd
                                      -- * Slices
                                      , row
                                      , col
                                      , rowWithout
                                      , colWithout
                                      , diag
                                      , at
                                      -- * Operations
                                      , reindex
                                      , replace
                                      , (+!)
                                      , (-!)
                                      , (*!)
                                      , (/!)
                                      , (*.)
                                      , transpose
                                      , nullifyRow
                                      , nullifyColumn
                                      , minor
                                      , norm
                                      , merge
                                      , mergeWith
                                     ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)
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

-- local helper to build a Tensor containing an empty Map
empty :: Tensor a
empty = Matrix M.empty (0,0)

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

-- | Checks if the 'Tensor' is square, i.e. it has the same number 
-- of rows and columns.
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

-- | Same as 'row' but removes the element located at the position given
-- by the second argument.
rowWithout :: Int -> Int -> Tensor a -> Tensor a
rowWithout i j (Matrix m (r,c)) 
    | i > r || i <= 0 = Empty
    | j > c || j <= 0 = Empty 
    | otherwise       = Matrix m' (1,c-1)
    where
      m' = M.fromList $ map (first $ first (const 1)) $ M.toList mf
      mf = M.delete (i,j) $ M.filterWithKey (\k _ -> fst k == i) m

-- | Same as 'col' but removes the element located at the position given
-- by the second argument.
colWithout :: Int -> Int -> Tensor a -> Tensor a
colWithout i j (Matrix m (r,c)) 
    | i > r || i <= 0 = Empty
    | j > c || j <= 0 = Empty 
    | otherwise       = Matrix m' (r-1,1)
    where
      m' = M.fromList $ map (first $ second (const 1)) $ M.toList mf
      mf = M.delete (j,i) $ M.filterWithKey (\k _ -> snd k == i) m




-- | Extracts the diagonal of a 'Tensor' if it is square. The result is a
-- square 'Tensor' with values only on the diagonal.
diag :: Tensor a -> Tensor a
diag (Matrix m (r,c)) | r /= c    = Empty
                      | otherwise = Matrix (go [1..r] M.empty) (r,c)
                      where
                        go [] m' = m'
                        go (x:xs) m' = case M.lookup (x,x) m of
                                         Nothing -> go xs m'
                                         Just v -> go xs (M.insert (x,x) v m')
diag _ = Empty

-- | Retrieves element at given 'Index'. 
-- Warning: no bounds check (in purpose).
at :: (Num a) => Tensor a -> Index -> a
at (Matrix m _) (i,j) = fromMaybe 0 $ M.lookup (i,j) m
at Empty _ = 0

-- | Moves a coefficient from the original index to a new one. If a
-- coefficient is present at the destination index, it is replaced by the new one.
reindex :: Index -> Index -> Tensor a -> Tensor a
reindex o n@(ni, nj) t@(Matrix m (i,j)) = 
    case M.lookup o m of
      Nothing -> t
      Just x -> Matrix (M.insert n x (M.delete o m)) (max i ni, max j nj)
reindex _ _ Empty = Empty

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

-- | The 'nullifyRow' function sets all coefficients from a row to zero.
-- If the coefficient is out of bounds the original 'Tensor' is returned.
nullifyRow :: Int -> Tensor a -> Tensor a
nullifyRow i t@(Matrix m (r,c)) = 
    if i > r then t 
    else Matrix (M.filterWithKey (\k _ -> fst k /= i) m) (r,c)
nullifyRow _ Empty = Empty

-- | The 'nullifyColumn' function sets all coefficients from a column to zero.
-- If the coefficient is out of bounds the original 'Tensor' is returned.
nullifyColumn :: Int -> Tensor a -> Tensor a
nullifyColumn i t@(Matrix m (r,c)) = 
    if i > c then t 
    else Matrix (M.filterWithKey (\k _ -> snd k /= i) m) (r,c)
nullifyColumn _ Empty = Empty

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

-- | Replaces an existing element at given 'Index'. For occasional use only, 
-- as its speed depends on the size of the 'Tensor'.
replace :: Tensor a -> Index -> a -> Tensor a
replace (Matrix m rc) ij x = Matrix (M.insert ij x m) rc
replace Empty _ _ = Empty

extract :: Index -> Index -> Tensor a -> Tensor a 
extract (ri,rj) (ci,cj) (Matrix m (r,c)) = Matrix m' (rj-ri+1,cj-ci+1)
    where
      withinBounds index = fst index >= ri && fst index <= rj && 
                           snd index >= ci && snd index <= cj
      m' = M.mapKeys (first (+(1-ri))) $ M.mapKeys (second (+(1-ci))) mw
      mw = M.filterWithKey (\k _ -> withinBounds k) m

vecProd :: Num a => Tensor a -> Tensor a -> Tensor a
vecProd m@(Matrix _ (r,1)) (Matrix n (1,c)) = Matrix (go [1..c] M.empty) (r,c)
    where
      go [] t = t
      go (i:is) t = go is $ M.union t t'
          where
            t' = M.map (*x) $ M.mapKeys (first $ const i) n
            x = m `at` (i,1)
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
    | otherwise = Matrix (M.singleton (1,1) (m *. n)) (1,1)
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
  fromInteger n = Matrix (M.singleton (1,1) $ fromInteger n) (1,1)
  signum = fmap signum
  (+) = add
  (*) = mult

infixl 7 *!
-- | Elementwise multiplication
(*!) :: Num a => a -> Tensor a -> Tensor a
(*!) x = fmap (*x)

infixl 7 /!
-- | Elementwise division.
(/!) :: (Fractional a, Num a) => Tensor a -> a -> Tensor a
(/!) t x = fmap (/x) t

infixl 6 +!
-- | Elementwise addition.
(+!) :: Num a => a -> Tensor a -> Tensor a
(+!) x = fmap (+x)

infixl 6 -!
-- | Elementwise substraction.
(-!) :: Num a => a -> Tensor a -> Tensor a
(-!) x = fmap (\y -> y-x)

-- merge t1 into t1, ie merge left into right
merge :: Tensor a -> Tensor a -> Tensor a
merge Empty m = m
merge m Empty = m
merge (Matrix m (rm,cm)) (Matrix n (rn,cn)) = Matrix (M.union n m) (max rm rn, max cm cn)

mergeAt :: Index -> Tensor a -> Tensor a -> Tensor a
mergeAt _ Empty m = m
mergeAt _ m Empty = m
mergeAt (i,j) (Matrix m (rm,cm)) (Matrix n (rn,cn)) = Matrix (mn) (max rm rn, max cm cn)
    where
      mn = M.union (M.mapKeys (first (+(i-1))) $ M.mapKeys (second (+(j-1))) n) m
      
mergeWith :: (a -> a -> a) -> Tensor a -> Tensor a -> Tensor a
mergeWith f (Matrix m (rm,cm)) (Matrix n (rn,cn)) = Matrix (M.unionWith f n m) (max rm rn, max cm cn)
mergeWith _ Empty m = m
mergeWith _ m Empty = m

-- | Checks if 'Tensor' is Symmetric Positive Definite, i.e. 'Tensor' is: 
-- symmetric /and/
-- all the diagonal entries are positive /and/
-- each diagonal entry is greater than the sum of the absolute values of 
-- all other entries in the corresponding column.
spd :: (Eq a, Ord a, Num a) => Tensor a -> Bool
spd m = symmetric m && positive m && definite m

-- | Checks if 'Tensor' is symmetric.
symmetric :: Eq a => Tensor a -> Bool
symmetric m = transpose m == m

-- are all the diagonal entries positive ?
positive :: (Ord a, Num a) => Tensor a -> Bool
positive = all (> 0) . diags

diags :: Tensor a -> [a]
diags = map snd . toList . diag                 

-- is each diagonal entry greater than the sum of the absolute values 
-- of all other entries in the corresponding column ?
-- test against twice the diagonal term > sum of whole column
definite :: (Ord a, Num a) => Tensor a -> Bool
definite m = let ds = map (*2) $ diags m
                 sums = [sum $ map (abs . snd) $ toList (col c m) | c <- [1..cols m]]
             in and $ zipWith (>) ds $ sums

-- | LU decomposition of a 'Tensor'.
lu :: (Eq a, Ord a, Num a, Fractional a) => Tensor a -> (Tensor a, Tensor a)
lu Empty = (Empty, Empty)
lu t = if not $ isSquare t then (Empty, Empty)
       else let rt = rows t in lud t [1..rt] rt empty empty
           
lud :: (Eq a, Ord a, Num a, Fractional a) => Tensor a -> [Int] -> Int -> Tensor a -> Tensor a -> (Tensor a, Tensor a)
lud _ [] _ l u = (l,u) 
lud t (i:is) d l u = let u11 = t `at` (1,1)
                         u12 = extract (1,1) (2,d) t
                         l21 = fmap (/u11) $ extract (2,d) (1,1) t
                         l' = merge (fromInteger 1) l21
                         u' = merge (fromList [((1,1), u11)]) u12
                         a22 = extract (2,d) (2,d) t
                         minorLU = a22 - l21 * u12
                     in lud minorLU is d (mergeAt (i,i) l' l) (mergeAt (i,i) u' u)

