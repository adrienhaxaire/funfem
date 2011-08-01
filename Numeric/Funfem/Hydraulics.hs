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



-- | Creates the elementary stiffness matrix for hydraulics
hydraulicStiffness :: Element -> Array DIM2 Double
hydraulicStiffness el = M.multiplyMM bt (M.multiplyMM k b)
  where 
    b = differentiate el
    bt = R.transpose b
    k = R.map (*(permeability el)) $ fromList (Z:.(2::Int):.(2::Int)) [1,0,0,1]

permeability :: Element -> Double
permeability el = matPropertyFromName (elemMaterial el) "permeability"
