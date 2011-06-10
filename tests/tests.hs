{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

-- here we specify that 'myLast' should return exactly the same result
-- as 'last' for any given 'xs'
prop_myLast xs = myLast xs == last xs

-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)
