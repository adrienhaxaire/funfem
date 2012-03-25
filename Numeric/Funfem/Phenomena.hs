---------------------------------------------------------------------------------- 
-- |
-- Module : Numeric.Funfem.Phenomena
-- Copyright : (c) Adrien Haxaire 2011-2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
--

module Numeric.Funfem.Phenomena (
                                 flux
                                 ,storage
                                 ,source
                                ) where

import Data.List (transpose, nub)
import qualified Data.Map as M

import Numeric.Funfem.Algebra.Polynomials 
import Numeric.Funfem.Elements

-- | Matrix resulting from the product between the transposed of the
-- gradient, a constant, and the gradient.  This function is quite handy when
-- dealing with fluxes, as in Fourier's law for example.
flux :: Element a => Double -> a -> [[Shape]]
flux x el = tmcm (grad el) x

-- | Matrix resulting from the product between the transposed of the
-- shape functions, a constant, and the shape functions. Useful for storage 
-- terms, like heat capacity for example.
storage :: Element a => Double -> a -> [[Shape]]
storage x el = tmcm (shape el) x

-- | Matrix resulting from the product between the transposed of the
-- shape functions and a constant. Useful for source terms,
-- like a volumetric heat source for example.
source :: Element a => Double -> a -> [[Shape]]
source x el = transpose $ shapeConst (shape el) x

-- TMCM : Transposed of Matrix, times a Constant, times same Matrix
tmcm :: [[Shape]] -> Double -> [[Shape]]
tmcm m x = multLists (transpose m) $ shapeConst m x

-- multiply a Shape matrix by a constant
shapeConst :: [[Shape]] -> Double -> [[Shape]]
shapeConst ss x = [[M.map (*x) p | p <- s] | s <- ss] 



