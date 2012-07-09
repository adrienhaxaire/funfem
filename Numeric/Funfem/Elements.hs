---------------------------------------------------------------------------------- 
-- |
-- Module : Numeric.Funfem.Elements
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@haxaire.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Elements where

import qualified Data.Map as M

data Node = Node {number :: Int, coordinates :: [Double]} 
            deriving (Eq, Ord, Show)

type Material = M.Map String Double

class Element a where
    nodes :: a -> [Node]
    material :: a -> Material

-- | Yields the node numbers of an element
nodeNumbers :: Element a => a -> [Int]
nodeNumbers = map number . nodes

-- | Determines the dimension (1D, 2D, etc) of the element based on
-- the number of coordinates.
dimension :: Element a => a -> Int  
dimension = length . coordinates . head . nodes

-- | Retrieves the value of an element material property. 
property :: Element a => String -> a -> Maybe Double
property s el = M.lookup s $ material el

-- | Helper function to create a 'Material' from an association list.
mkMaterial :: [(String, Double)] -> Material
mkMaterial = M.fromList

-- | Linear triangular element.
data Tri3 = Tri3 {nodesTri3 :: [Node], matTri3 :: Material}
          deriving (Eq, Ord, Show)

instance Element Tri3 where
    nodes = nodesTri3
    material = matTri3

-- | The area of a 'Tri3'.
area :: Tri3 -> Double
area el = 0.5 * ((x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1))
    where  
      [[x1,y1],[x2,y2],[x3,y3]] = map coordinates $ nodes el

 

