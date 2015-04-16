module Solver where

import qualified Data.Vector.Unboxed as V

type Vector = V.Vector Double
type Row = Int
type Col = Int
type Size = Int
type Idx = Int

-- sparse matrices for later optimisation    
data Matrix = Matrix { matrixData :: Vector
                     , matrixSize :: Size}
            deriving (Eq, Ord, Show)

matrix :: Size -> Matrix
matrix n = Matrix { matrixData = V.replicate n 0
                  , matrixSize = n}

fromVector :: Vector -> Matrix
fromVector v = Matrix { matrixData = v
                      , matrixSize = n}
    where
      n = floor . sqrt . fromIntegral . V.length $ v

idx :: Size -> (Row, Col) -> Idx
idx n (i, j) = n * i + j
               
row :: Row -> Matrix -> Vector
row i m = V.slice (idx n (i,0)) n (matrixData m)
    where
      n = matrixSize m
          


