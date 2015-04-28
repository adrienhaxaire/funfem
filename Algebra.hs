module Algebra where

import qualified Data.Vector as V

instance Num a => Num (V.Vector a) where
   negate = V.map negate
   u + v = V.zipWith (+) u v
   u * v = V.zipWith (*) u v
   abs = V.map abs
   signum _ = undefined
   fromInteger _ = undefined

data Matrix a = Matrix { rows :: V.Vector (V.Vector a)
                       , rowsInit :: V.Vector Int}
              deriving (Eq, Ord, Show)
                      
instance Functor Matrix where
    fmap f (Matrix rs is) = Matrix { rows = V.map (V.map f) rs
                                   , rowsInit = is}
            

fromList :: [V.Vector a] -> Matrix a
fromList as = Matrix { rows = V.fromList as
                     , rowsInit = V.replicate (length as) 0}

row :: Matrix a -> Int -> V.Vector a
row a i = (rows a) V.! i

rowInit :: Matrix a -> Int -> Int
rowInit m i = (rowsInit m) V.! i

size :: Matrix a -> Int
size = V.length . rows

inner :: Num a => V.Vector a -> V.Vector a -> a
inner u v = V.sum $ V.zipWith (*) u v

infixl 9 !.
(!.) :: Num a => V.Vector a -> V.Vector a -> a
u !. v = inner u v

norm :: (Num a, Floating a) => V.Vector a -> a
norm v = sqrt $ inner v v

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

