module Solver where

import qualified Data.Vector as V
    
import Algebra


cg :: Matrix Double -> V.Vector Double -> V.Vector Double
cg a b = go r0 p0 u0
    where
    u0 = V.replicate (size a) 0
    r0 = b
    p0 = r0

    tolerance :: Double
    tolerance = 1.0e-4

    go u r p = if norm r' < tolerance then u
               else go u' r' p'
                   where
                     alpha = (r !. r) / ((a !* p) !. p)
                     u' = u - fmap (* alpha) p
                     r' = r - fmap (* alpha) (a !* p)
                     beta = (r' !. r') / (r !. r)
                     p' = r' + fmap (* beta) p

{-
v1, v2, v3 :: V.Vector Double
v1 = V.fromList [7,4,6]
v2 = V.fromList [4,8,4]
v3 = V.fromList [6,4,9]

a :: Matrix Double
a = fromList [v1, v2, v3]

unitMatrix :: Matrix Double    
unitMatrix = fromList [V.fromList [1,0,0], V.fromList [0,1,0], V.fromList [0,0,1]]
-}


