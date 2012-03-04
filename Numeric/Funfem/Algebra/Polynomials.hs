---------------------------------------------------------------------------------- 
-- |
-- Module : Numeric.Funfem.Algebra.Polynomials
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Algebra.Polynomials where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe

type Polynomial = M.Map String Double

type Eval = M.Map Char Double

occ :: Char -> String -> Double 
occ c [] = 0.0
occ c (x:xs) = let n = if c == x then 1.0 else 0.0 in n + occ c xs


differentiate :: Polynomial -> Char -> Polynomial
differentiate p c = M.mapKeys (L.delete c) $
                    M.mapWithKey (\ k v -> (occ c k) * v) $
                    M.filterWithKey (\k _ -> c `elem` k) p

eval :: Polynomial -> Eval -> Double
eval p e = M.foldl (+) 0.0 $ evals p e

evals :: Polynomial -> Eval -> Polynomial
evals p e = evals' p e $ M.keys e
    where
      evals' q ev [] = q
      evals' q ev (c:cs) = evals' (evalOne q ev c) ev cs                 

evalOne :: Polynomial -> Eval -> Char -> Polynomial
evalOne p e c = M.mapWithKey f p
    where
      f k v = v * (x ** (occ c k))
      x = fromJust $ M.lookup c e
 

p1 :: Polynomial
p1 = M.fromList [("",1.0), ("x",2.0), ("y",3.0), ("xy",4.0), ("xxy",5.0)]

e1 :: Eval
e1 = M.fromList [('x',1.0), ('y',1.0)]

e2 :: Eval
e2 = M.fromList [('x',2.0), ('y',2.0)]
