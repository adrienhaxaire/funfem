---------------------------------------------------------------------------------- 
-- |
-- Module : Input
-- Copyright : (c) Adrien Haxaire 2011
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--
-- Reads JSON formatted input data
--

module Input where

import Text.JSON
import Elements


data Input = Input [Node] [Element] [Material] [BoundaryCondition]
             deriving (Eq, Ord, Show)

instance JSON Input where
  readJSON object = do
    obj <- readJSON object
    nodes <- valFromObj "nodes" obj
    elements <- valFromObj "elements" obj
    materials <- valFromObj "materials" obj
    bcs <- valFromObj "boundaries" obj
    return (Input nodes elements materials bcs)
  showJSON (Input nodes elements materials bcs) = makeObj [("nodes", showJSON nodes)
                                                      ,("elements", showJSON elements)
                                                      ,("materials", showJSON materials)
                                                      ,("boundaries", showJSON bcs)]
                                              
nodes :: Input -> [Node]                                              
nodes (Input nodes _ _ _) = nodes

elements :: Input -> [Element]
elements (Input _ elements _ _) = elements 

materials :: Input -> [Material]
materials (Input _  _ materials _) = materials 

bcs :: Input -> [BoundaryCondition]
bcs (Input _ _ _ bcs) = bcs 

inputFromResult :: Result Input -> Input
inputFromResult (Ok input) = input
inputFromResult (Error _) = Input [] [] [] []
