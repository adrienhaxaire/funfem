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

module Numeric.Funfem.Stiffnesses (toGlobalMatrix) where

import Data.List as L hiding (transpose)
import Data.Map as M

import Numeric.Funfem.Elements
import Numeric.Funfem.Vector as V 
import Numeric.Funfem.Matrix 

type Index = (Int,Int)
type Stiffness = M.Map (Int,Int) Double 

-- | Assembles the global stiffness matrix from the elementary
-- stiffness constructor and the target list of elements
toGlobalMatrix :: (Element -> Matrix) -> [Element] -> Matrix
toGlobalMatrix elemStiff els = toMatrix $ toGlobal elemStiff els


toGlobal :: (Element -> Matrix) -> [Element] ->  Stiffness
toGlobal elemStiff els = M.unions $ L.map (elemToGlobal elemStiff) els


toMatrix :: Stiffness -> Matrix
toMatrix s = fromVectors $ vs 
  where
    vs = [V.fromList [at (i,j) s | j <- [1..smax]] | i <- [1..smax]]
    larger = fst $ findMax s
    smax = max (fst larger) (snd larger)
    

at :: Index -> Stiffness -> Double
at index s = case M.lookup index s of {Just val -> val; Nothing -> 0.0}


elemToGlobal :: (Element -> Matrix) -> Element -> Stiffness
elemToGlobal elemStiff el = indexToGlobal localIndices globalIndices matrix initialize
  where
    matrix = elemStiff el
    nodes = L.map nodeNumber (elemNodes el)
    nbNodes = length nodes
    globalIndices = [(i,j) | i <- nodes, j <- nodes]
    localIndices = [(i,j) | i <- [1..nbNodes], j <- [1..nbNodes]]
    initialize = M.singleton (1,1) 0.0 :: Stiffness

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


-- [Element [Node (0.0,0.0) 1,Node (1.0,0.0) 2,Node (1.0,1.0) 3] 1 (Material "sand" [Property "permeability" 1.0] 1),Element [Node (1.0,0.0) 2,Node (1.0,1.0) 3,Node (0.0,1.0) 4] 2 (Material "sand" [Property "permeability" 1.0] 1)]
