import qualified Data.Vector as V

import Numeric.Funfem

main = do   
  let a = V.fromList [V.fromList [1.0, 0.0, 0.0], V.fromList [-1.0, 2.0, -1.0], V.fromList [0.0, 1.0, -1.0]]
  let f = V.fromList [10.0, 0.0, 0.0]
  let res = cg a f
  print res