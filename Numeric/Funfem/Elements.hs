---------------------------------------------------------------------------------- 
-- |
-- Module : Elements
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

data Node = Node {coordinates :: Point, nodeNumber :: Int} 
            deriving (Eq, Ord, Show)

type Material = M.Map String Double

-- | Type class for an element. 
class Element a where
  nodes :: a -> [Node]
  material :: a -> Material
  shape :: a -> [[Double] -> Double] 
  
-- | Linear line element.
data Lin2 = Lin2 {nodesLin2 :: [Node], matLin2 :: Material}
            deriving (Eq, Ord, Show)
            
coorsLin2 :: Lin2 -> [Double]
coorsLin2 el = map (head . coordinates) $ nodesLin2 el

lengthLin2 :: Lin2 -> Double
lengthLin2 el = let [x1,x2] = coorsLin2 el in abs (x1 - x2) 

shapeLin2 :: Lin2 -> [[Double] -> Double]
shapeLin2 el = [f1, f2] 
  where
    l = lengthLin2 el 
    f1 [x] = 1-x/l
    f2 [x] = x/l

instance Element Lin2 where
  nodes = nodesLin2
  material = matLin2
  shape = shapeLin2




  

n1 = Node [0.0] 1  
n2 = Node [1.0] 2
mat = M.fromList [("conductivity", 1.0)]

el1 = Lin2 [n1,n2] mat

