---------------------------------------------------------------------------------- 
-- |
-- Module : Stiffnesses
-- Copyright : (c) Adrien Haxaire 2011
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Stiffnesses (
  toGlobal
  ,toMatrix
  ,toGlobalMatrix
  ,setStiffnessRow
  ,setStiffness
  ,applyBoundaryConditions
  ,buildSystem
--  ,elementaryStiffness
  ) where

import Data.List as L hiding (transpose)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Numeric.Funfem.Elements
import qualified Numeric.Funfem.Matrix as FM
import qualified Numeric.Funfem.Vector as V
import Numeric.Funfem.BoundaryConditions
import Numeric.Funfem.RightHandSide (buildRHS)

--import Numeric.Funfem.ShapeFunctions

type Index = (Int,Int)
type Stiffness = M.Map (Int,Int) Double 

-- | Wrapper to quickly build a system 
buildSystem :: (Element -> FM.Matrix) -> [Element] -> [BoundaryCondition] -> (FM.Matrix, V.Vector)
buildSystem elemStiff els bcs = (stiffness, rhs)
  where
    stiffness = toMatrix $ applyBoundaryConditions bcs $ toGlobal elemStiff els
--    stiffness = toMatrix $ toGlobal elemStiff els
    rhs = buildRHS bcs (fst $ FM.dim stiffness)

-- | Assembles the global stiffness matrix from the elementary
-- stiffness constructor and the target list of elements
toGlobalMatrix :: (Element -> FM.Matrix) -> [Element] -> FM.Matrix
toGlobalMatrix elemStiff els = toMatrix $ toGlobal elemStiff els

toMatrix :: Stiffness -> FM.Matrix
toMatrix s = FM.fromLists [[at (i,j) s | j <- [1..smax]] | i <- [1..smax]]  where smax = size s
           
at :: Index -> Stiffness -> Double
at index s = fromMaybe 0.0 $ M.lookup index s

size :: Stiffness -> Int    
size s = uncurry max (fst $ M.findMax s) 

toGlobal :: (Element -> FM.Matrix) -> [Element] ->  Stiffness
toGlobal elemStiff els = M.unionsWith (+) $ L.map (elemToGlobal elemStiff) els

elemToGlobal :: (Element -> FM.Matrix) -> Element -> Stiffness
elemToGlobal elemStiff el = addToGlobal localIndices globalIndices matrix initialize
  where
    matrix = elemStiff el
    nodes = L.map nodeNumber (elemNodes el)
    nbNodes = length nodes
    globalIndices = [(i,j) | i <- nodes, j <- nodes]
    localIndices = [(i,j) | i <- [1..nbNodes], j <- [1..nbNodes]]
    initialize = M.singleton (1,1) 0.0 :: Stiffness

addToGlobal :: [Index] -> [Index] -> FM.Matrix -> Stiffness -> Stiffness
addToGlobal [] _ _ s = s  
addToGlobal _ [] _ s = s
addToGlobal (l:ls) (g:gs) m s = addToStiffness g (FM.atIndex m l) s'
  where
    s' = addToGlobal ls gs m s

addToStiffness :: Index -> Double -> Stiffness -> Stiffness    
addToStiffness (i,j) val s 
  | i <= 0    = s
  | j <= 0    = s
  | otherwise = M.insertWith' (+) (i,j) val s 


applyBoundaryConditions :: [BoundaryCondition] -> Stiffness -> Stiffness
applyBoundaryConditions [] s = s
applyBoundaryConditions (bc:bcs) s = applyBoundaryConditions bcs s'
  where
    s' = setStiffness (row,row) 1.0 $ setStiffnessRow (row,1) 0.0 s
    row = nodeNumber $ bcNode bc


setStiffnessRow :: Index -> Double -> Stiffness -> Stiffness
setStiffnessRow (row,col) val s 
  | col < n   = setStiffnessRow (row,col+1) val s'
  | otherwise = setStiffness (row,n) val s
  where
    n = size s
    s' = setStiffness (row,col) val s

setStiffness :: Index -> Double -> Stiffness -> Stiffness    
setStiffness (i,j) val s 
  | i <= 0    = s
  | j <= 0    = s
  | otherwise = M.insert (i,j) val s 


{- here for debugging

elementaryStiffness :: Element -> FM.Matrix
elementaryStiffness el = (FM.transpose $ tri3' el) * (permeability `FM.multSM` (tri3' el))
  where
    permeability = matPropertyFromName mat "permeability"
    mat = elemMaterial el

-- [Element [Node (0.0,0.0) 1,Node (1.0,0.0) 2,Node (1.0,1.0) 3] 1 (Material "sand" [Property "permeability" 1.0] 1),Element [Node (1.0,1.0) 3,Node (0.0,1.0) 4,Node (0.0,0.0) 1] 2 (Material "sand" [Property "permeability" 1.0] 1)]

-}