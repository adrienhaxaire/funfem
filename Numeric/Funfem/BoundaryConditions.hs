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

import Numeric.Funfem.Elements

-- | Needs the list of nodes affected by the boundary conditions,
-- i.e. no Neumann BC handled yet
data BoundaryCondition = BoundaryCondition Name [Node] Value
                         deriving (Eq, Ord, Show)
                                  
instance JSON BoundaryCondition where
  readJSON object = do
    obj <- readJSON object
    name <- valFromObj "name" obj
    nodes <- valFromObj "nodes" obj
    value <- valFromObj "value" obj
    return (BoundaryCondition name nodes value)
  showJSON (BoundaryCondition name nodes value) = makeObj [("name", showJSON name)
                                                          ,("nodes", showJSON nodes)
                                                          ,("value", showJSON value)]

bcName :: BoundaryCondition -> Name
bcName (BoundaryCondition name _ _) = name

bcNodes :: BoundaryCondition -> [Node]
bcNodes (BoundaryCondition _ nodes _) = nodes

bcValue :: BoundaryCondition -> Value
bcValue (BoundaryCondition _ _ value) = value

-- [BoundaryCondition "Dirichlet" [Node (0.0,0.0) 1,Node (1.0,0.0) 2] 10.0]

