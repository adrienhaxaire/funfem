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
import Elements

{-
-- | Creates the elementary stiffness matrix
elementStiffness :: Element -> Material -> Array DIM2 Double
elementStiffness el mat = R.map (*permeability') (bc el)
  where permeabilty' = (permeabilty mat) / (4.0 * (det ))
        
permeabilty :: Material -> Double
permeabilty mat = getPropertyValue property
  where property = filter (\n -> (getPropertyName n) == "permeabilty") (getMaterialProperties mat)

det :: 

bc :: 
-}