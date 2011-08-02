{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All


import Numeric.Funfem.Elements as E


instance Arbitrary E.Node where
  arbitrary = do
    x1 <- choose (-100.0, 100.0) :: Gen Double
    y1 <- choose (-100.0, 100.0) :: Gen Double
    n1 <- choose (1, 100) :: Gen Int
    return (E.Node (x1, y1) n1)                         

prop_nodeFromExtractor n = (E.Node (nodeCoordinates n) (nodeNumber n)) == n


instance Arbitrary E.Property where
  arbitrary = do
    name <- arbitrary 
    value <- choose (-1.0e31, 1.0e31) :: Gen Double
    return (E.Property name value)

prop_propertyFromExtractor p = (E.Property (propName p) (propValue p)) == p


instance Arbitrary E.Material where
  arbitrary = do
    name <- arbitrary
    numProps <- choose (0,100) :: Gen Int
    props <- vectorOf numProps arbitrary
    number <- arbitrary
    return (E.Material name props number)

prop_materialFromExtractor m = (E.Material (matName m) (matProperties m) (matNumber m)) == m


instance Arbitrary E.Element where
  arbitrary = do
    numNodes <- choose (0,30) :: Gen Int
    nodes <- vectorOf numNodes arbitrary
    number <- arbitrary
    material <- arbitrary
    return (E.Element nodes number material)
    
prop_elementFromExtractor e = (E.Element (elemNodes e) (elemNumber e) (elemMaterial e)) == e    


main = $(quickCheckAll)