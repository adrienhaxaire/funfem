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
module Numeric.Funfem.Mesh where 

import Data.List (nub)

import Numeric.Funfem.Elements


-- | Mesh data type
data Mesh a = Mesh [a] 

fromList :: Element a => [a] -> Mesh a
fromList es = Mesh es

toList :: Element a => Mesh a -> [a]
toList (Mesh es) = es

meshElements :: Element a => Mesh a -> [a]
meshElements = toList

meshNodes :: Element a => Mesh a -> [Node]
meshNodes = concat . map nodes . toList 

meshNodesUnique :: Element a => Mesh a -> [Node]
meshNodesUnique = nub . meshNodes

