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
-- Functions to create theelement stiffness matrices and the global stiffness matrix
--
module Numeric.Funfem.Stiffnesses (
       differenciate
       ) where

import Data.Array.Repa as R
import Numeric.Funfem.Elements

-- | Differenciation matrix, usually noted B
differenciate :: Element -> Array DIM2 Double
differenciate el = fromList (Z:.(2::Int):.(3::Int)) (Prelude.map (/area') b13c13')
  where 
    b13c13' = b13c13 el
    area' = area el

area :: Element -> Double
area el = (x2y3 + x3y1 + x1y2 - x2y1 - x3y2 - x1y3) / 2.0   
  where 
    co = coor el
    x2y3 = (co !! 3) * (co !! 6)  
    x3y1 = (co !! 5) * (co !! 2)
    x1y2 = (co !! 1) * (co !! 4) 
    x2y1 = (co !! 3) * (co !! 2) 
    x3y2 = (co !! 5) * (co !! 4) 
    x1y3 = (co !! 1) * (co !! 6)

b13c13 :: Element -> [Double]
b13c13 el = [b1, b2, b3, c1, c2, c3]
  where 
    co = coor el
    b1 = (co !! 4) - (co !! 6)
    b2 = (co !! 6) - (co !! 2)
    b3 = (co !! 2) - (co !! 4)
    c1 = (co !! 5) - (co !! 3)
    c2 = (co !! 1) - (co !! 5)
    c3 = (co !! 3) - (co !! 1)

coor :: Element -> [Double]  
coor = coor' . elemNodes
       where 
         coor' [] = []
         coor' (n:ns) = x n : [y n] Prelude.++ coor' ns

x :: Node -> Double
x = fst . nodeCoordinates        

y :: Node -> Double
y = snd . nodeCoordinates

