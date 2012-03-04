---------------------------------------------------------------------------------- 
-- |
-- Module : Numeric.Funfem.Assembly
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Assembly where

import Data.List (transpose)
import qualified Data.Map as M
import Data.List

import Numeric.Funfem.Algebra.Matrix
import Numeric.Funfem.Elements

-- | Derivative of function with respect to variable position
df :: Int      -- ^ @n@ variable number
      -> Shape -- ^ @f@ function to differentiate
      -> Shape 
df n f = let h = 0.001 in 
  \x -> (f (take (n-1) x ++ [x !! (n-1) + h] ++ drop n x) - f x) / h

-- | Integration function using Simpson's rule
intf :: Double  -- ^ @a@ lower bound of integral
     -> Double  -- ^ @b@ upper bound of integral
     -> Shape   -- ^ @f@ function to integrate
     -> Double
intf a b f = (f [a] + 4 * f [(a+b)/2.0] + f [b]) * (b-a)/ 6.0

grad :: Element a => a -> [[Shape]]
grad el = [map (df n) $ shapes el | n <- take (dimension el) $ iterate (+1) 1] 
tgrad :: Element a => a -> [[Shape]]
tgrad = transpose . grad

-- here for temporary debugging
n1 = Node [0.0] 1  
n2 = Node [1.0] 2
n3 = Node [2.0] 3
mat = M.fromList [("conductivity", 1.0)]

el1 = Lin2 [n1,n2] mat
el2 = Lin2 [n2,n3] mat



-- tgrad * conductivity * grad
