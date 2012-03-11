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

import Data.Maybe (fromJust)

import Numeric.Funfem.Algebra.Matrix
import Numeric.Funfem.Elements
import Numeric.Funfem.Mesh
import Numeric.Funfem.Phenomena

-- need type Equation to carry all the terms (phenomena)

