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

import Numeric.Funfem.Algebra.Polynomials
import Data.List (nub)

type Point = [Double]
type Shape = Polynomial

data Node = Node {coordinates :: Point, nodeNumber :: Int} 
            deriving (Eq, Ord, Show)

type Material = M.Map String Double

-- | Helper function to create a 'Material' from an association list.
mkMaterial :: [(String, Double)] -> Material
mkMaterial = M.fromList

-- | Type class for an element. 
class Element a where
  nodes :: a -> [Node]
  material :: a -> Material
  shapes :: a -> [Shape] 
  
-- | Determines the dimension (1D, 2D, etc) of the element based on
-- the number of coordinates per Point.
dimension :: Element a => a -> Int  
dimension = length . coordinates . head . nodes

-- | Retrieves the value of an element material property. 
property :: Element a => String -> a -> Maybe Double
property s el = M.lookup s $ material el

-- | Single row matrix containing the shape functions.
shape :: Element a => a -> [[Shape]]
shape el = [shapes el]

-- | The 'grad' function calculates the gradient of the shape functions.
grad :: Element a => a -> [[Shape]]
grad el = [map (\p -> differentiate p var) (shapes el) | var <- vars el]
    where
      vars = nub . concat . map variables . shapes

-- | The 'evaluations' function generates the list of 'Evaluation' for an 
-- 'Element'.
evaluations el = [mkEvaluation $ zip vars (coordinates node) | node <- nodes el]
    where
      vars = variables $ head $ shapes el


-- | Evaluates a 'Polynomial' at given 'Node'.
evaluateAtNode :: Polynomial -> Node -> Double
evaluateAtNode p n = evaluate p eval
    where
      eval = mkEvaluation $ zip (variables p) (coordinates n)

-- | Evaluates 'Polynomial' nested lists at given 'Node'.
evaluateListsAtNode :: [[Polynomial]] -> Node -> [[Double]]
evaluateListsAtNode m n = evaluateLists m eval 
    where
      eval = mkEvaluation $ zip vars (coordinates n)
      vars = variables $ head $ head m

-- | Evaluates a 'Polynomial' at given 'Point'.
evaluateAtPoint :: Polynomial -> Point -> Double
evaluateAtPoint p point = evaluate p eval
    where
      eval = mkEvaluation $ zip (variables p) point

-- | Evaluates 'Polynomial' nested lists at given 'Point'.
evaluateListsAtPoint :: [[Polynomial]] -> Point -> [[Double]]
evaluateListsAtPoint m point = evaluateLists m eval 
    where
      eval = mkEvaluation $ zip vars point
      vars = variables $ head $ head m


-- ------------------------ Elements declarations ----------------------------
-- | Linear line element.
data Lin2 = Lin2 {nodesLin2 :: [Node], matLin2 :: Material}
            deriving (Eq, Ord, Show)
           
coorsLin2 :: Lin2 -> [Double]
coorsLin2 el = map (head . coordinates) $ nodesLin2 el

lengthLin2 :: Lin2 -> Double
lengthLin2 el = let [x1,x2] = coorsLin2 el in abs (x1 - x2) 

shapesLin2 :: Lin2 -> [Shape]
shapesLin2 el = [shape1, shape2]
    where
      l = lengthLin2 el 
      shape1 = mkPolynomial [("",1.0),("x",-1.0/l)]
      shape2 = mkPolynomial [("x",1.0/l)] 

instance Element Lin2 where
  nodes = nodesLin2
  material = matLin2
  shapes = shapesLin2


-- | Linear triangular element.
data Tri3 = Tri3 {nodesTri3 :: [Node], matTri3 :: Material}
          deriving (Eq, Ord, Show)

coorsTri3 :: Tri3 -> [[Double]]
coorsTri3 = map coordinates . nodesTri3

areaTri3 :: Tri3 -> Double
areaTri3 el =  0.5 * ((x2-x1) * (y3-y1) - (x3-x1) * (y2-y1))
    where  
      [[x1,y1],[x2,y2],[x3,y3]] = coorsTri3 el

shapesTri3 :: Tri3 -> [Shape]
shapesTri3 el = map mkShape [n1,n2,n3]
    where
      [[x1,y1],[x2,y2],[x3,y3]] = coorsTri3 el
      n1 = [("",x2*y3 - x3*y2), ("x",y2-y3), ("y",x3-x2)]
      n2 = [("",x3*y1 - x1*y3), ("x",y3-y1), ("y",x1-x3)]
      n3 = [("",x1*y2 - x2*y1), ("x",y1-y2), ("y",x2-x1)]
      mkShape = M.map (*twoAreas) . mkPolynomial
      twoAreas = (x2-x1) * (y3-y1) - (x3-x1) * (y2-y1)

instance Element Tri3 where
  nodes = nodesTri3
  material = matTri3
  shapes = shapesTri3

