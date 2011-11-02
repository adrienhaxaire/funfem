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

module Numeric.Funfem.Solver (cg) where

import Numeric.Funfem.Vector

eps :: Double
eps = 1.0e-3

-- | Solves Ax = b. Arguments are passed in this order. No first guess on x is made, so 
-- it should be initialized first.
cg :: Matrix -> Vector -> Vector -> Vector
cg a x b = if norm r <= eps then x else cg' a x r r r
  where
    r = b - a `dot` x

cg' :: Matrix -> Vector -> Vector -> Vector -> Vector -> Vector
cg' a x r z p = if norm r' <= eps then x' else cg' a x' r' z' p'
  where
    alpha = (r .* z) / (p .* (a `dot` p))
    beta = (z' .* r') / (z .* r)
    x' = x + vmap (*alpha) p
    r' = r - vmap (*alpha) (a `dot` p)
    z' = r'    
    p' = z'+ vmap (*beta) p





{- taken from wikipedia:

x0 = 0 (or better)
r0 = b - Ax0
z0 = M-1 r0
p0 = z0
k = 0

loop:
alpha(k) = (r(k).z(k))/(p(k).A.p(k))
x(k+1) = x(k) + alpha(k).p(k)
r(k+1) = r(k) - aplha(k).A.p(k)
check on r(k+1)
z(k+1) = M^-1 . r(k+1)
beta(k+1) = z(k+1).r(k+1)/z(k).r(k)
p(k+1) = z(k+1) + beta(k)p(k)
k = k + 1
end loop

-}

