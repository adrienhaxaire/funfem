{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
---------------------------------------------------------------------------------- 
-- |
-- Module : Numeric.Funfem.Elements
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Elements where 

import qualified Data.Map as M

type Point = [Double]

type Shape = Point -> Double

data Node = Node {coordinates :: Point, nodeNumber :: Int} 
            deriving (Eq, Ord, Show)

type Material = M.Map String Double

-- | Type class for an element. 
class Element a where
  nodes :: a -> [Node]
  material :: a -> Material
  shapes :: a -> [Shape] 
  
-- | Determines the dimension (1D, 2D, etc) of the element based on
-- the number of coordinates per Point
dimension :: Element a => a -> Int  
dimension = length . coordinates . head . nodes

-- ------------------------ Elements declarations ----------------------------
-- | Linear line element.
data Lin2 = Lin2 {nodesLin2 :: [Node], matLin2 :: Material}
            deriving (Eq, Ord, Show)
            
coorsLin2 :: Lin2 -> [Double]
coorsLin2 el = map (head . coordinates) $ nodesLin2 el

lengthLin2 :: Lin2 -> Double
lengthLin2 el = let [x1,x2] = coorsLin2 el in abs (x1 - x2) 

shapesLin2 :: Lin2 -> [Shape]
shapesLin2 el = let l = lengthLin2 el in [\x -> 1.0 - head x /l, \x -> head x /l]

instance Element Lin2 where
  nodes = nodesLin2
  material = matLin2
  shapes = shapesLin2

