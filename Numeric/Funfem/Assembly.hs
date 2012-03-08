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

module Numeric.Funfem.Assembly where

import Data.List (transpose, nub)
import qualified Data.Map as M

import Numeric.Funfem.Algebra.Matrix
import Numeric.Funfem.Algebra.Polynomials
import Numeric.Funfem.Elements

