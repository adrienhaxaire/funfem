---------------------------------------------------------------------------------- 
-- |
-- Module : Numeric.Funfem.Assembly
-- Copyright : (c) Adrien Haxaire 2011-2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
--

module Numeric.Funfem.Assembly (
                                toMatrix
                               )where

import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as M (lookup, map)

import Numeric.Funfem.Algebra.Matrix
import Numeric.Funfem.Algebra.Polynomials
import Numeric.Funfem.Elements
import Numeric.Funfem.Mesh
import Numeric.Funfem.Phenomena

thermalFlux :: Element a => a -> [[Shape]]
thermalFlux el = flux conductivity el
    where
      conductivity = fromJust $ M.lookup "conductivity" $ material el 

toMatrix :: Element a => (a -> [[Shape]]) -> a -> Matrix
toMatrix ph el = let sh = ph el in 
                 if areConstant sh 
                 then matrix [[fromMaybe 0.0 $ M.lookup "" s | s <- ss ]| ss <- sh]
                 else error "Variable shape functions not yet implemented"
