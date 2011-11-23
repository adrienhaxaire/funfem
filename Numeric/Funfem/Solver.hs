---------------------------------------------------------------------------------- 
-- |
-- Module : Solver
-- Copyright : (c) Adrien Haxaire 2011
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Solver where

import Numeric.Funfem.Vector
import Numeric.Funfem.Matrix

import qualified Data.List as L

eps :: Double
eps = 1.0e-3

-- | Solves Ax = b. Arguments are passed in this order. No first guess on x is made, so 
-- it should be initialized first.
cg :: Matrix -> Vector -> Vector -> Vector
cg a x b = if norm r <= eps then x else cg' a x r r r
  where
    r = b - multMV a x

cg' :: Matrix -> Vector -> Vector -> Vector -> Vector -> Vector
cg' a x r z p = if norm r' <= eps then x' else cg' a x' r' z' p'
  where
    alpha = (r .* z) / (p .* multMV a p)
    beta = (z' .* r') / (z .* r)
    x' = x + vmap (*alpha) p
    r' = r - vmap (*alpha) (multMV a p)
    z' = r'    
    p' = z'+ vmap (*beta) p

-- | LU decomposition and back substitution

upper :: [[Double]] -> [[Double]]
upper [] = []
upper x = (L.head upped) : upper minored
  where
    upped = up x
    minored = minor up x

up :: [[Double]] -> [[Double]]
up [] = []
up (r:rs) = r : up' r rs 

up' :: [Double] -> [[Double]] -> [[Double]]
up' _ [] = []
up' r (l:ls) = zipWith (-) l (L.map (*h) r) : up' r ls
  where
    h = L.head l / L.head r 
    
    
lower :: [[Double]] -> [[Double]]
lower [] = []
lower m = [reverse l | l <- lowered]
  where
    lowered = reverse $ low $ L.transpose m

low :: [[Double]] -> [[Double]]
low [] = []
low x = L.map (/h) (L.head x) : low minored
  where
    h = (L.head . L.head) x  
    minored = minor id x

minor :: ([a] -> [[b]]) -> [a] -> [[b]]
minor _ [] = []
minor f xs = L.tail [L.tail x | x <- f xs]

