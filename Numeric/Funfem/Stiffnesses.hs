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

module Numeric.Funfem.Stiffnesses where

import Data.List as L hiding (transpose)
import Data.Map as M

import Numeric.Funfem.Elements
import Numeric.Funfem.Matrix 
import Numeric.Funfem.ShapeFunctions

type Index = (Int,Int)
type Stiffness = M.Map (Int,Int) Double 

-- todo : convert stiffness to matrix
-- add init

initialize :: Stiffness
initialize = M.singleton (1,1) 0.0 :: Stiffness 

toGlobal :: (Element -> Matrix) -> [Element] ->  Stiffness
toGlobal elemStiff els = M.unions $ L.map (elemToGlobal elemStiff) els

elemToGlobal :: (Element -> Matrix) -> Element -> Stiffness
elemToGlobal elemStiff el = indexToGlobal localIndices globalIndices matrix initialize
  where
    matrix = elemStiff el
    nodes = L.map nodeNumber (elemNodes el)
    nbNodes = length nodes
    globalIndices = [(i,j) | i <- nodes, j <- nodes]
    localIndices = [(i,j) | i <- [1..nbNodes], j <- [1..nbNodes]]


indexToGlobal :: [Index] -> [Index] -> Matrix -> Stiffness -> Stiffness
indexToGlobal [] _ _ s = s  
indexToGlobal _ [] _ s = s
indexToGlobal (l:ls) (g:gs) m s = toStiffness g (atIndex m l) s'
  where
    s' = indexToGlobal ls gs m s


toStiffness :: Index -> Double -> Stiffness -> Stiffness    
toStiffness (i,j) val s 
  | i <= 0    = s
  | j <= 0    = s
  | otherwise = M.insertWith' (+) (i,j) val s 


--toMatrix :: Stiffness -> Matrix
--toMatrix s = fromVectors $ 



-- only here for development
elementaryStiffness :: Element -> Matrix
elementaryStiffness el = (transpose $ tri3' el) * (permeability `multSM` (tri3' el))
  where
    permeability = matPropertyFromName mat "permeability"
    mat = elemMaterial el


-- [Element [Node (0.0,0.0) 1,Node (1.0,0.0) 2,Node (1.0,1.0) 3] 1 (Material "sand" [Property "permeability" 1.0] 1),Element [Node (1.0,0.0) 2,Node (1.0,1.0) 3,Node (0.0,1.0) 4] 2 (Material "sand" [Property "permeability" 1.0] 1)]
