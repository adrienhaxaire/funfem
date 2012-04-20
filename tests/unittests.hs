import Test.HUnit

import Numeric.Funfem.Algebra.Tensor

test1 = TestCase (assertEqual "" minorR1 minorT1 )
    where
      minorR1 = vector [4.0]
      minorT1 = minor (1,1) $ matrix [[1.0,2.0],[3.0,4.0]]










--test1 = TestCase (assertEqual "toto" True False)
--main = runTestTT test1

