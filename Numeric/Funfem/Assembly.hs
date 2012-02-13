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

-- | Derivative of function with respect to variable position
df :: Int  -- ^ @n@ variable number
      -> ([Double] -> Double)  -- ^ @f@ function to differentiate
      -> [Double] -> Double 
df n f = let h = 0.001 in 
  \x -> (f (take (n-1) x ++ [x !! (n-1) + h] ++ drop n x) - f x) / h

-- | Integration function using Simpson's rule
intf :: ([Double] -> Double) -- ^ @f@ function to integrate
        -> Double -- ^@a@ lower bound of integral
        -> Double -- ^@b@ upper bound of integral
        -> Double
intf f a b = (f [a] + 4 * f [(a+b)/2.0] + f [b]) * (b-a)/ 6.0




f1 :: [Double] -> Double
f1 [x] = x

f2 :: [Double] -> Double
f2 [x, y]  = 1 + x + y