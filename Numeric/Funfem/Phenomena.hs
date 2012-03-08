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
flux :: Element a => a -> Double -> [[Shape]]
flux el x = tmcm (grad el) x

-- | Matrix resulting from the product between the transposed of the
-- shape functions, a constant, and the shape functions. Useful for storage 
-- terms, like heat capacity for example.
storage :: Element a => a -> Double -> [[Shape]]
storage el x = tmcm (shape el) x

-- | Matrix resulting from the product between the transposed of the
-- shape functions and a constant. Useful for source terms,
-- like a volumetric heat source for example.
source :: Element a => a -> Double -> [[Shape]]
source el x = transpose $ shapeConst (shape el) x

-- TMCM : Transposed of Matrix, times a Constant, times same Matrix
tmcm :: [[Shape]] -> Double -> [[Shape]]
tmcm m x = multMat (transpose m) $ shapeConst m x

-- multiply a Shape matrix by a constant
shapeConst :: [[Shape]] -> Double -> [[Shape]]
shapeConst ss x = [[M.map (*x) p | p <- s] | s <- ss] 


{-
e1 = mkEvaluation [('x',1.0)]

-- here for temporary debugging
n1 = Node [0.0] 1  
n2 = Node [1.0] 2
n3 = Node [2.0] 3
mat = mkMaterial [("conductivity", 1.0)]

el1 = Lin2 [n1,n2] mat
el2 = Lin2 [n2,n3] mat
-}


-- tgrad * conductivity * grad
