---------------------------------------------------------------------------------- 
-- |
-- Module : Elements
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Elements where

import Numeric.Funfem.Stiffnesses

class Element a where
  integrate :: a -> Stiffness