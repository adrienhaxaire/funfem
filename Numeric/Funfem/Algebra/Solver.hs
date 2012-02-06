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

module Numeric.Funfem.Algebra.Solver (
  module Numeric.Funfem.Algebra.Solver.CG
  ,module Numeric.Funfem.Algebra.Solver.LU
  ) where

import Numeric.Funfem.Algebra.Solver.CG
import Numeric.Funfem.Algebra.Solver.LU
