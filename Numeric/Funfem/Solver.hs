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

module Numeric.Funfem.Solver (
  cg
  ,luSolve
  ) where

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


-- | Solves Ax = b using LU decomposition and substitutions.   
luSolve :: Matrix -> Vector -> Vector
luSolve m b = fromList $ findX upped [] $ findY lowered [] b' 
  where
    upped = upper m'
    lowered = lower m'
    m' = fromMatrix' m 
    b' = fromVector b


-- find y / Ly = b
findY :: [[Double]] -> [Double] -> [Double] -> [Double]
findY [] y _ = y
findY _ y [] = y
findY (_:ls) [] (b:bs) = findY ls [b] bs
findY (l:ls) y (b:bs) = findY ls y' bs
  where
    y' = y L.++ [b - L.sum left]
    left = zipWith (*) l y
    

-- find x / Ux = y
findX :: [[Double]] -> [Double] -> [Double] -> [Double]    
findX [] x _ = x
findX u [] y = findX (L.init u) x0 (L.init y) 
  where x0 = [L.last y / L.head (L.last u)] 
findX us x ys = findX (L.init us) (x':x) (L.init ys)      
  where
    x' = (y - L.sum left) / uh  
    left = L.tail $ zipWith (*) u (1:x)
    y = L.last ys
    u = L.last us
    uh = L.head u 


-- stores only non zero values
upper :: [[Double]] -> [[Double]]
upper [] = []
upper x = L.head upped : upper minored
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
    
    
-- stores only non zero values    
lower :: [[Double]] -> [[Double]]
lower [] = []
lower m = L.reverse . rearrange . low $ m


rearrange :: [[Double]] -> [[Double]]
rearrange [] = []
rearrange m = arrange m : rearrange (minor' id m)
    where
      arrange = L.reverse . L.map L.last . L.transpose


low :: [[Double]] -> [[Double]]
low [] = []
low (l:ls) = L.map L.head (d:ds) : minored
  where
    h = L.head l 
    (d:ds) = (L.map . L.map) (/h) (l:ls)
    minored = low $ minor id $ up' d (d:ds)


minor :: ([a] -> [[b]]) -> [a] -> [[b]]
minor _ [] = []
minor f xs = L.tail [L.tail x | x <- f xs]

minor' :: ([a] -> [[b]]) -> [a] -> [[b]]
minor' _ [] = []
minor' f xs = L.init [L.init x | x <- f xs]

