---------------------------------------------------------------------------------- 
-- |
-- Module : Hydraulics
-- Copyright : (c) Adrien Haxaire 2011
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--
-- Stuff related to water
--
-- Material should provide only Darcy's permeablity. 
-- Will be more flexible as soon as I have something working.
--
--

module Numeric.Funfem.Hydraulics where

import Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix as M
import Numeric.Funfem.Elements
import Numeric.Funfem.Stiffnesses

-- | Creates the elementary stiffness matrix
elementStiffness :: Element -> Material -> Array DIM2 Double
elementStiffness el mat = M.multiplyMM bt (M.multiplyMM k b)
  where 
    b = differenciate el
    bt = R.transpose b
    k = R.map (*(permeability mat)) $ fromList (Z:.(2::Int):.(2::Int)) [1,0,0,1]
        
permeability :: Material -> Double
permeability mat = propValue $ head property
  where property = filter (\n -> (propName n) == "permeabilty") (matProperties mat)
