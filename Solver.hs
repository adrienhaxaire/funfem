module Solver where

import qualified Data.Vector.Unboxed as V
-- import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed.Mutable as MV

{-
lu :: V.Vector (V.Vector Double) -> (V.Vector (V.Vector Double), V.Vector (V.Vector Double))
lu vvs = go 0 0 vvs m0 m0
    where
      v0 = V.replicate n (0.0 :: Double)
      m0 = V.replicate n v0
      n = V.length vvs
      go i j a l u | i == n = (l, u)
                   | j == n = go (i + 1) 0 a l u 
                   | otherwise = go i (j + 1) a l' u' 
                                 where
                                   l' = V.modify

look at MVector.write, M.modify
http://stackoverflow.com/questions/21138189/data-vector-modify-creates-vector-copies-on-each-iteration

-}

{-
process :: [Int] -> Vector Int -> Vector Int
process [] v = v
process (x:xs) v = process xs $ Vector.modify modify v
  where
  modify mv = do
    old <- MVector.read mv x
    MVector.write mv x (old + 1)
-}

modif v = do
  old <- MV.read v 0
  MV.write v 0 (old + 1)


V.modify (\v -> MV.write v 1 2) v1


