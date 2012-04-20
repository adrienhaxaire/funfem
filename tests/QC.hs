{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
import Test.QuickCheck
import Test.QuickCheck.All

import Numeric.Funfem.Algebra.Tensor

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





main :: IO Bool
main = $(quickCheckAll)
