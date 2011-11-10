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
import Data.Map as M hiding (size, (!))

import Numeric.Funfem.Elements
import Numeric.Funfem.Vector as V
import Numeric.Funfem.Matrix 
import Numeric.Funfem.ShapeFunctions

type Stiffness = M.Map (Int,Int) Double 

elementaryStiffness :: Element -> Matrix
elementaryStiffness el = (transpose $ tri3' el) * (permeability `multSM` (tri3' el))
  where
    permeability = matPropertyFromName mat "permeability"
    mat = elemMaterial el
    

{-
inject :: Matrix -> Stiffness -> Stiffness
inject (Matrix []) s = s
inject m s = let s' = injectVector (i, size v) v' s 
                      v' = V.last v
             in if size v > 0 then injectVector i (V.tail v) s' else s  
  fromVectors 
  where
    vs = fromMatrix m
-}


-- first Int to specify row number
injectVector :: Int -> Vector -> Stiffness -> Stiffness
injectVector 0 _ s = s
injectVector i (Vector []) s = s  
injectVector i v s = if size v > 0 then injectVector i (V.init v) s' else s
  where s' = toStiffness (i, size v) (V.last v) s 
    
toStiffness :: (Int,Int) -> Double -> Stiffness -> Stiffness    
toStiffness (i,j) val s 
  | i <= 0    = s
  | j <= 0    = s
  | otherwise = M.insertWith' (+) (i,j) val s 



-- [Element [Node (0.0,0.0) 1,Node (1.0,0.0) 2,Node (1.0,1.0) 3] 1 (Material "sand" [Property "permeability" 1.0] 1),Element [Node (1.0,0.0) 2,Node (1.0,1.0) 3,Node (0.0,1.0) 4] 2 (Material "sand" [Property "permeability" 1.0] 1)]
