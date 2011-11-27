---------------------------------------------------------------------------------- 
-- |
-- Module : BoundaryConditions
-- Copyright : (c) Adrien Haxaire 2011
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.BoundaryConditions where

import Text.JSON
import Data.List as L

import Numeric.Funfem.Elements

-- | Needs the list of nodes affected by the boundary conditions,
-- i.e. no Neumann BC handled yet
data BoundaryCondition = BoundaryCondition Name Node Value
                         deriving (Eq, Ord, Show)
                                  
instance JSON BoundaryCondition where
  readJSON object = do
    obj <- readJSON object
    name <- valFromObj "name" obj
    node <- valFromObj "node" obj
    value <- valFromObj "value" obj
    return (BoundaryCondition name node value)
  showJSON (BoundaryCondition name node value) = makeObj [("name", showJSON name)
                                                         ,("node", showJSON node)
                                                         ,("value", showJSON value)]

bcName :: BoundaryCondition -> Name
bcName (BoundaryCondition name _ _) = name

bcNode :: BoundaryCondition -> Node
bcNode (BoundaryCondition _ node _) = node

bcNodeNumbers :: [BoundaryCondition] -> [Int]   
bcNodeNumbers bcs = L.map nodeNumber $ L.map bcNode bcs

bcValue :: BoundaryCondition -> Value
bcValue (BoundaryCondition _ _ value) = value

bcValues :: [BoundaryCondition] -> [Double]
bcValues bcs = L.map bcValue bcs


-- [BoundaryCondition "Dirichlet" (Node (0.0,0.0) 1) 10.0, BoundaryCondition "Dirichlet" (Node (1.0,0.0) 2) 10.0]
