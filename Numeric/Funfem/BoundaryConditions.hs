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
import Data.Map as M
import Data.List as L

import Numeric.Funfem.Elements
import Numeric.Funfem.Vector as V

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

bcNodes :: BoundaryCondition -> Node
bcNodes (BoundaryCondition _ node _) = node

bcNodeNumbers :: [BoundaryCondition] -> [Int]   
bcNodeNumbers bcs = L.map nodeNumber $ L.map bcNodes bcs

bcValue :: BoundaryCondition -> Value
bcValue (BoundaryCondition _ _ value) = value

bcValues :: [BoundaryCondition] -> [Double]
bcValues bcs = L.map bcValue bcs



-- | temporary building of the right hand side
buildRHS :: [BoundaryCondition] -> Int -> Vector
buildRHS bcs n = V.fromList $ [at i nvs | i <- [1..n]]
  where
    nodes = bcNodeNumbers bcs
    values = bcValues bcs
    nvs = M.fromList $ zip nodes values
    
at :: (Ord k, Fractional a) => k -> Map k a -> a
at node candidates = case M.lookup node candidates of {Just val -> val; Nothing -> 0.0}


-- [BoundaryCondition "Dirichlet" (Node (0.0,0.0) 1) 10.0, BoundaryCondition "Dirichlet" (Node (1.0,0.0) 2) 10.0]
