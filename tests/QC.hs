{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All
 
main :: IO Bool
main = $(quickCheckAll)
