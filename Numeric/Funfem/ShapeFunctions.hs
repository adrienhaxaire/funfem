---------------------------------------------------------------------------------- 
-- |
-- Module : ShapeFunctions
-- Copyright : (c) Adrien Haxaire 2011
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.ShapeFunctions where

import Data.List as L

import Numeric.Funfem.Elements
import Numeric.Funfem.Vector


tri3 :: Element -> [Vector]
tri3 el = zipWith (*) coorsV shapes
  where
    coors = [nodeCoordinates node | node <- elemNodes el]
    coorsV = [fromList [1, fst cs, snd cs] | cs <- coors]        
    coorsM = fromVectors coorsV
    perms = [[1,2,3], [2,3,1], [3,1,2]]
    shapes = [fromList $ L.map (/twoAreas) [det (butRowColumn 1 i coorsM) | i <- is] | is <- perms] 
    twoAreas = det coorsM




