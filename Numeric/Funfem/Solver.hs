---------------------------------------------------------------------------------- 
-- |
-- Module : Solver
-- Copyright : (c) Adrien Haxaire 2012
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
  ) where

import qualified Data.Vector as V
import Numeric.Funfem.Algebra as A

-- | Solves Ax = b. Arguments are passed in this order. No first guess on x is made, so 
-- it should be initialized first.
cg :: Matrix -> Vector -> Vector
cg a b = cg' a x0 b b b
  where
    x0 = V.replicate (V.length a) 0.0
    
cg' :: Matrix -> Vector -> Vector -> Vector -> Vector -> Vector
cg' a x r z p = if norm r' <= eps then x' else cg' a x' r' z' p'
  where
    alpha = (r .* z) / (p .* multMV a p)
    beta = (z' .* r') / (z .* r)
    x' = x + V.map (*alpha) p
    r' = r - V.map (*alpha) (multMV a p)
    z' = r' -- placeholder for preconditioner  
    p' = z'+ V.map (*beta) p
    
eps :: Double
eps = 1.0e-3

{-
-- | Solves Ax = b using LU decomposition and substitutions.   
luSolve :: Matrix -> Vector -> Vector
luSolve m b = fromList $ findX upped [] $ findY lowered [] b' 
  where
    upped = upper m'
    lowered = lower m'
    m' = fromMatrix' m 
    b' = fromVector b


-- find y / Ly = b
-- find x / Ux = y

-}