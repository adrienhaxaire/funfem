---------------------------------------------------------------------------------- 
-- |
-- Module : Numeric.Funfem.Algebra.Solver.CG
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@haxaire.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Algebra.Solver.CG (cg) where

import Numeric.Funfem.Algebra.Tensor

-- | Solves Ax = b using the preconditioned conjugate gradient method. 
-- Unstable as check for SPD not yet implemented.
cg :: Tensor Double -- ^ A, the left-hand side matrix of the system
   -> Tensor Double -- ^ b, the right-hand side vector
   -> Tensor Double -- ^ x, the resulting vector
cg a b = if spd a then cg' a x0 r0 d0 m
         else cg' taa x0 tar0 tad0 m
  where
    x0 = vector $ replicate (rows b) 0.0
    r0 = b - a * x0
    m = preconditioner a
    d0 = m * r0
    ta = transpose a
    taa = ta * a
    tar0 = ta * b - taa * x0
    tad0 = m * tar0


cg' :: Tensor Double -> Tensor Double -> Tensor Double 
    -> Tensor Double -> Tensor Double -> Tensor Double
cg' a x r d m = if norm r' <= 1.0e-3 then x' else cg' a x' r' d' m
  where
    alpha = (r *. (m * r)) / (d *. (a * d))
    x' = x + alpha *! d
    r' = r - alpha *! (a * d)
    beta' = (r' *. (m * r')) / (r *. (m * r))
    d' = m * r' + beta' *! d

-- Jacobi preconditioner
preconditioner :: Tensor Double -> Tensor Double
preconditioner = fmap (\x -> 1.0 / x) . diag 

