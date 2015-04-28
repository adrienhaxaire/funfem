module Solver where

import qualified Data.Vector as V

import Algebra

cgWithTolerance :: Matrix Double -> V.Vector Double -> Double -> V.Vector Double
cgWithTolerance a b tolerance = go u0 r0 p0
    where
    u0 = V.replicate (size a) 0
    r0 = b
    p0 = r0

    go u r p = if norm r < tolerance then u
               else go u' r' p'
                   where
                     alpha = (r !. r) / ((a !* p) !. p)
                     u' = u + fmap (* alpha) p
                     r' = r - fmap (* alpha) (a !* p)
                     beta = (r' !. r') / (r !. r)
                     p' = r' + fmap (* beta) p


cg :: Matrix Double -> V.Vector Double -> V.Vector Double
cg a b = cgWithTolerance a b 1.0e-4




