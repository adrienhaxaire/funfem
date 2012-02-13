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
import qualified Data.Vector as V

import Numeric.Funfem.Elements

type Function = [Double] -> Double

-- | Derivative of function with respect to variable position
df :: Int                     -- ^ @n@ variable number
      -> Function -- ^ @f@ function to differentiate
      -> Function 
df n f = let h = 0.001 in 
  \x -> (f (take (n-1) x ++ [x !! (n-1) + h] ++ drop n x) - f x) / h

-- | Integration function using Simpson's rule
intf :: Function -- ^ @f@ function to integrate
        -> Double            -- ^@a@ lower bound of integral
        -> Double            -- ^@b@ upper bound of integral
        -> Double
intf f a b = (f [a] + 4 * f [(a+b)/2.0] + f [b]) * (b-a)/ 6.0



grad :: Element a => a -> [[Function]]
grad el = [map (df n) $ shape el | n <- take (dimension el) $ iterate (+1) 1] 

tgrad :: Element a => a -> [[Function]]
tgrad = transpose . grad

{-
f1 :: Function
f1 [x] = x

f2 :: Function
f2 [x, y]  = 1 + x + y

f3 :: Function
f3 [x, y]  = 1 - x - y


add :: Function -> Function -> Function
add f g = \x -> f x + g x
-}