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
  ,buildRHS
  ,adaptGlobalToRHS
  ,setStiffnessRow
  ,setStiffness
  ) where

import Data.List as L hiding (transpose)
import qualified Data.Map as M

import Numeric.Funfem.Elements
import Numeric.Funfem.BoundaryConditions
import qualified Numeric.Funfem.Vector as V 
import Numeric.Funfem.Matrix 

-- debug:
import Numeric.Funfem.ShapeFunctions


type Index = (Int,Int)
type Stiffness = M.Map (Int,Int) Double 

--buildSystem


-- | Assembles the global stiffness matrix from the elementary
-- stiffness constructor and the target list of elements
toGlobalMatrix :: (Element -> Matrix) -> [Element] -> Matrix
toGlobalMatrix elemStiff els = toMatrix $ toGlobal elemStiff els


-- | temporary building of the right hand side
buildRHS :: [BoundaryCondition] -> Int -> V.Vector
buildRHS bcs n = V.fromList $ [at i nvs | i <- [1..n]]
  where
    nodes = bcNodeNumbers bcs
    values = bcValues bcs
    nvs = M.fromList $ zip nodes values
    at i nvs = case M.lookup i nvs of {Just val -> val; Nothing -> 0.0}


-- | update global stiffness matrix according to RHS: set 1 where BC is defined
adaptGlobalToRHS :: V.Vector -> Stiffness -> Stiffness
adaptGlobalToRHS rhs global = adaptGlobalToList (V.fromVector rhs) global

adaptGlobalToList :: [Double] -> Stiffness -> Stiffness 
adaptGlobalToList [] s = s
adaptGlobalToList list@(d:ds) s 
  | d == 0.0  = adaptGlobalToList ds s
  | otherwise = adaptGlobalToList ds updated
  where
    i = L.length list - size s
    updated = adaptToOneBC i s

adaptToOneBC :: Int -> Stiffness -> Stiffness
adaptToOneBC pos s = M.insert (pos,pos) 1.0 s'
  where
    s' = setStiffnessRow pos 0.0 s  

toGlobal :: (Element -> Matrix) -> [Element] ->  Stiffness
toGlobal elemStiff els = M.unions $ L.map (elemToGlobal elemStiff) els


toMatrix :: Stiffness -> Matrix
toMatrix s = fromVectors $ vs 
  where
    vs = [V.fromList [at (i,j) s | j <- [1..smax]] | i <- [1..smax]]
    smax = size s
    

at :: Index -> Stiffness -> Double
at index s = case M.lookup index s of {Just val -> val; Nothing -> 0.0}


size :: Stiffness -> Int    
size s = let last = fst $ M.findMax s in max (fst last) (snd last)


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


setStiffness :: Index -> Double -> Stiffness -> Stiffness    
setStiffness (i,j) val s 
  | i <= 0    = s
  | j <= 0    = s
  | otherwise = M.insert (i,j) val s 


setStiffnessRow :: Int -> Double -> Stiffness -> Stiffness
setStiffnessRow row val s = setStiffnessRow' (row,1) val s


setStiffnessRow' :: Index -> Double -> Stiffness -> Stiffness
setStiffnessRow' (row,col) val s 
  | col < n   = setStiffnessRow' (row,col+1) val s'
  | otherwise = setStiffness (row,n) val s
  where
    n = size s
    s' = setStiffness (row,col) val s
    

setStiffnessCol :: Int -> Double -> Stiffness -> Stiffness
setStiffnessCol col val s = setStiffnessCol' (1,col) val s


setStiffnessCol' :: Index -> Double -> Stiffness -> Stiffness
setStiffnessCol' (row,col) val s 
  | row < n   = setStiffnessCol' (row+1,col) val s'
  | otherwise = setStiffness (n,col) val s
  where
    n = size s
    s' = setStiffness (row,col) val s


  
      
  
  


-- [Element [Node (0.0,0.0) 1,Node (1.0,0.0) 2,Node (1.0,1.0) 3] 1 (Material "sand" [Property "permeability" 1.0] 1),Element [Node (1.0,0.0) 2,Node (1.0,1.0) 3,Node (0.0,1.0) 4] 2 (Material "sand" [Property "permeability" 1.0] 1)]
