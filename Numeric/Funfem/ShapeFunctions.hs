---------------------------------------------------------------------------------- 
-- |
-- Module : ShapeFunctions
-- Copyright : (c) Adrien Haxaire 2011
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.ShapeFunctions where

import Numeric.Funfem.Elements
import Numeric.Funfem.Vector as V
import Numeric.Funfem.Matrix

-- | Interpolation function for 3-noded triangle
tri3 :: Element -> Matrix
tri3 el = multSM (1/twoAreas) $ fromVectors [V.fromList n | n <- [n1,n2,n3]] 
  where
    [(x1,y1),(x2,y2),(x3,y3)] = [nodeCoordinates node | node <- elemNodes el]
    twoAreas = (x2-x1) * (y3-y1) - (x3-x1) * (y2-y1)
    n1 = [x2*y3-x3*y2, y2-y3, x3-x2]
    n2 = [x3*y1-x1*y3, y3-y1, x1-x3]
    n3 = [x1*y2-x2*y1, y1-y2, x2-x1]

-- | Derivative of the tri3 interpolation
tri3' :: Element -> Matrix
tri3' el = fromVectors [bs,cs]
  where
    [(x1,y1),(x2,y2),(x3,y3)] = [nodeCoordinates node | node <- elemNodes el]
    twoAreas = (x2-x1) * (y3-y1) - (x3-x1) * (y2-y1)
    bs = fromList [y2-y3, y3-y1, y1-y2]
    cs = fromList [x3-x2, x1-x3, x2-x1]
