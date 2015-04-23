module Algebra where

import qualified Data.Vector as V

data Matrix a = Matrix { rows :: V.Vector (V.Vector a)
                       , rowsInit :: V.Vector Int}
              deriving (Eq, Ord, Show)

                       
fromList :: [V.Vector a] -> Matrix a
fromList as = Matrix { rows = V.fromList as
                     , rowsInit = V.replicate (length as) 0}

row :: Matrix a -> Int -> V.Vector a
row a i = (rows a) V.! i

rowInit :: Matrix a -> Int -> Int
rowInit m i = (rowsInit m) V.! i

inner :: Num a => V.Vector a -> V.Vector a -> a
inner u v = V.sum $ V.zipWith (*) u v

infixl 9 !.
(!.) :: Num a => V.Vector a -> V.Vector a -> a
u !. v = inner u v

multMV :: Num a => Matrix a -> V.Vector a -> V.Vector a
multMV a v = V.fromList $ go [] 0
    where
      n = V.length $ rows a 
      go xs i | i == n    = reverse xs 
              | otherwise = go (x:xs) (i + 1)
              where
                x = inner r $ V.slice start end v
                r = row a i
                start = rowInit a i
                end = V.length r - start
infixl 9 !*
(!*) :: Num a => Matrix a -> V.Vector a -> V.Vector a
m !* v = multMV m v

