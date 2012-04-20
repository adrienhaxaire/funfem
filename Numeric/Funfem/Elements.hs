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

import qualified Numeric.Funfem.Algebra.Polynomials as P
import Data.List (nub)

type Point = [Double]
type Shape = P.Polynomial

data Node = Node {nodeNumber :: Int, coordinates :: Point} 
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
  integration :: a -> P.Polynomial -> Double

-- | Determines the dimension (1D, 2D, etc) of the element based on
-- the number of coordinates per Point.
dimension :: Element a => a -> Int  
dimension = length . coordinates . head . nodes

-- | Retrieves the value of an element material property. 
property :: Element a => String -> a -> Maybe Double
property s el = M.lookup s $ material el

-- | The 'grad' function calculates the gradient of the shape functions.
grad :: Element a => a -> [[Shape]]
grad el = [map (\p -> P.differentiate p var) (shapes el) | var <- vars el]
    where
      vars = nub . concat . map P.variables . shapes

-- | The 'evaluations' function generates the list of 'Evaluation' for an 
-- 'Element'.
evaluations el = [P.mkEval $ zip vars (coordinates node) | node <- nodes el]
    where
      vars = P.variables $ head $ shapes el

-- | The 'constShape' function turns a Double into a constant 'Shape' function.
constShape :: Double -> Shape
constShape x = P.mkPoly [("",x)]

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
      shape1 = P.mkPoly [("",1.0),("x",-1.0/l)]
      shape2 = P.mkPoly [("x",1.0/l)] 

integrateLin2 :: Lin2 -> P.Polynomial -> Double
integrateLin2 el p = P.evaluate (P.integrateBetween p 'x' 0.0 len) eval 
    where 
      len = lengthLin2 el
      eval = P.mkEval []

instance Element Lin2 where
  nodes = nodesLin2
  material = matLin2
  shapes = shapesLin2
  integration = integrateLin2

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
      mkShape = M.map (/twoAreas) . P.mkPoly
      twoAreas = (x2-x1) * (y3-y1) - (x3-x1) * (y2-y1)

instance Element Tri3 where
  nodes = nodesTri3
  material = matTri3
  shapes = shapesTri3
  integration = undefined
