{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All


import Numeric.Funfem.Elements as E
import Numeric.Funfem.Vector

-- Elements
instance Arbitrary E.Node where
  arbitrary = do
    x1 <- choose (-100.0, 100.0) :: Gen Double
    y1 <- choose (-100.0, 100.0) :: Gen Double
    n1 <- choose (1, 100) :: Gen Int
    return (E.Node (x1, y1) n1)                         

prop_nodeFromExtractor :: E.Node -> Bool
prop_nodeFromExtractor n = (E.Node (nodeCoordinates n) (nodeNumber n)) == n


instance Arbitrary E.Property where
  arbitrary = do
    name <- arbitrary 
    value <- choose (-1.0e31, 1.0e31) :: Gen Double
    return (E.Property name value)

prop_propertyFromExtractor :: E.Property -> Bool
prop_propertyFromExtractor p = (E.Property (propName p) (propValue p)) == p


instance Arbitrary E.Material where
  arbitrary = do
    name <- arbitrary
    numProps <- choose (0,100) :: Gen Int
    props <- vectorOf numProps arbitrary
    number <- arbitrary
    return (E.Material name props number)

prop_materialFromExtractor :: E.Material -> Bool
prop_materialFromExtractor m = (E.Material (matName m) (matProperties m) (matNumber m)) == m


instance Arbitrary E.Element where
  arbitrary = do
    numNodes <- choose (0,30) :: Gen Int
    nodes <- vectorOf numNodes arbitrary
    number <- arbitrary
    material <- arbitrary
    return (E.Element nodes number material)
    
prop_elementFromExtractor :: E.Element -> Bool    
prop_elementFromExtractor e = (E.Element (elemNodes e) (elemNumber e) (elemMaterial e)) == e    

-- Vectors
instance Arbitrary Vector where
  arbitrary = do
    len <- choose (0,100) :: Gen Int
    list <- vectorOf len arbitrary
    return (Vector list)

prop_double_add_same_size :: Vector -> Bool  
prop_double_add_same_size v = size (v + v) == size v

-- Matrix
instance Arbitrary Matrix where
  arbitrary = do
    -- len <- choose (1,3) :: Gen Int
    let len = 4
    v1 <- vectorOf len arbitrary
    v2 <- vectorOf len arbitrary
    v3 <- vectorOf len arbitrary
    v4 <- vectorOf len arbitrary
    let vs = Prelude.map fromList [v1,v2,v3,v4]
    return (fromVectors vs)

prop_double_transpose :: Matrix -> Bool
prop_double_transpose m = (transpose.transpose) m == m

prop_transposed_det_equal :: Matrix -> Bool
prop_transposed_det_equal m = abs(det (transpose m) - det m) < 1.0e-3




main :: IO Bool
main = $(quickCheckAll)