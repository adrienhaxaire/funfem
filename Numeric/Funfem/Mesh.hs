---------------------------------------------------------------------------------- 
-- |
-- Module : Numeric.Funfem.Mesh
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--
module Numeric.Funfem.Mesh (
                            fromElements
                            ,meshElements
                            ,meshNodes
                            ,meshNodesUnique
                           )where 

import Data.List (nub)

import Numeric.Funfem.Elements


-- | Mesh data type
data Mesh a = Mesh [a] 

fromElements :: Element a => [a] -> Mesh a
fromElements es = Mesh es

meshElements :: Element a => Mesh a -> [a]
meshElements (Mesh es) = es

meshNodes :: Element a => Mesh a -> [Node]
meshNodes = concat . map nodes . meshElements

meshNodesUnique :: Element a => Mesh a -> [Node]
meshNodesUnique = nub . meshNodes

