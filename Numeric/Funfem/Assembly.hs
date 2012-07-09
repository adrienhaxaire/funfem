---------------------------------------------------------------------------------- 
-- |
-- Module : Numeric.Funfem.Assembly
-- Copyright : (c) Adrien Haxaire 2011-2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@haxaire.org>
-- Stability : experimental
-- Portabilty : not tested
--
--

module Numeric.Funfem.Assembly where

import Data.Maybe

import Numeric.Funfem.Elements
import Numeric.Funfem.Algebra.Tensor

data BoundaryCondition = Dirichlet [(Int, Double)]
                       deriving (Eq, Ord, Show)

assemble :: [Tri3] -> Double -> Tensor Double
assemble trgs x = go trgs empty
    where
      go [] r = r
      go (t:ts) r = go ts $ mergeWith (+) (toGlobal t x) r

stiffness :: Tri3 -> Double -> Tensor Double
stiffness t x = x *! (transpose b) * b /! (4 * area t)
    where
      b = matrix [bs, cs]
      [[x1,y1],[x2,y2],[x3,y3]] = map coordinates $ nodes t
      bs = [y2 - y3, y3 - y1, y1 - y2]
      cs = [x3 - x2, x1 - x3, x2 - x1]

toGlobal :: Tri3 -> Double -> Tensor Double
toGlobal t x = go oldIndices newIndices $ stiffness t x
    where
      go [] _ r = r
      go (o:os) (n:ns) r = go os ns $ reindex o n r

      indices l = concat [zip (repeat n) l | n <- l]
      oldIndices = indices [1,2,3]
      newIndices = indices $ nodeNumbers t


-- sort unknowns/knowns
sortBC :: Tensor Double -> BoundaryCondition -> (Tensor Double, Tensor Double)
sortBC t (Dirichlet bcs) = go t bcs (vector $ replicate (rows t) 0.0)
    where
      go [] k b = (k, b)
      go ((i,x):ixs) k b = go ixs k b



