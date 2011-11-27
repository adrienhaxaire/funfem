---------------------------------------------------------------------------------- 
-- |
-- Module : RightHandSide
-- Copyright : (c) Adrien Haxaire 2011
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.RightHandSide where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Numeric.Funfem.BoundaryConditions
import qualified Numeric.Funfem.Vector as V 

type RHS = M.Map Int Double

buildRHS :: [BoundaryCondition] -> Int -> V.Vector
buildRHS [] n = V.genVector n 0.0
buildRHS bcs n = V.fromList [rhs `at` i | i <- [1..n]] 
  where
    rhs = fromBoundaryConditions bcs


fromBoundaryConditions :: [BoundaryCondition] -> RHS
fromBoundaryConditions bcs = M.fromList $ zip nodes values
  where
    nodes = bcNodeNumbers bcs
    values = bcValues bcs
    
at :: RHS -> Int -> Double
at r i = fromMaybe 0.0 (M.lookup i r)

