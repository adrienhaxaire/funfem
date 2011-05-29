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
-- Conjugate Gradient solver
--

module Solver where

import Data.Array.Repa as R

eps :: Double
eps = 1.0e-3

{-

cg :: Array DIM2 Double -> Array DIM1 Double -> Array DIM1 Double
cg a b = if r0 <= eps then x else cg' a x r0 
  where 
    r0 = residue b a x

cg' :: Array DIM2 Double -> Array DIM1 Double -> Array DIM1 Double
 
-}  
  
-- | Residue needed to check the exit of the CG.
residue :: Array DIM1 Double -> Array DIM2 Double -> Array DIM1 Double -> Double
residue b a x = norm (b - a `multiplyMV` x)

norm :: Array DIM1 Double -> Double
norm  = sqrt . toScalar . R.sum . R.map (**2) 


-- | Didn't find it in the repa lib. Computes matrix * vector.
multiplyMV :: Array DIM2 Double -> Array DIM1 Double -> Array DIM1 Double
multiplyMV a x = R.sum $ a*x'
  where x' = extend (Z:.(dim::Int):.All) x
        dim = rank $ extent a


{-
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