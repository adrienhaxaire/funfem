{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
import Test.QuickCheck
import Test.QuickCheck.All

import Data.Monoid

import Numeric.Funfem.Algebra.Tensor
import Numeric.Funfem.Elements
import Numeric.Funfem.Mesh


-- Tensors
instance Arbitrary (Tensor Double) where
    arbitrary = do
      is <- vectorOf 10 arbitrary
      js <- vectorOf 10 arbitrary
      let idx = zip is js
      as <- vectorOf 10 arbitrary
      return (fromList $ zip idx as)

prop_transposedTwice :: Tensor Double -> Bool
prop_transposedTwice m = (transpose . transpose) m == m

prop_addTwice :: Tensor Double -> Bool
prop_addTwice m = m + m == fmap (*2) m

prop_mergeWithSame :: Tensor Double -> Bool
prop_mergeWithSame m = mergeWith (+) m m == m + m

-- Mesh
instance Arbitrary Triangle where
    arbitrary = do
      p <- arbitrary
      q <- arbitrary
      r <- arbitrary
      return $ triangle p q r

instance Arbitrary Mesh where
    arbitrary = do
      l <- vectorOf 100 arbitrary
      return $ mesh l

prop_monoidLeftMesh :: Mesh -> Bool
prop_monoidLeftMesh m = m `mappend` mempty == m

prop_monoidRightMesh :: Mesh -> Bool
prop_monoidRightMesh m = mempty `mappend` m == m

prop_monoidAssocMesh :: Mesh -> Mesh -> Mesh -> Bool
prop_monoidAssocMesh m1 m2 m3 = 
    (m1 `mappend` m2) `mappend` m3 == m1 `mappend` (m2 `mappend` m3)


main :: IO Bool
main = $(quickCheckAll)
