{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
-- Multivariate 'Polynomial's. Defined to represent shape functions, allowing
-- their exact multiplication, addition and integration. 
-- 
-- A barebone Num instance is provided, but the functions 'add', 'substract'
-- and 'mult' are provided for comodity.

module Numeric.Funfem.Algebra.Polynomials (
                                           Polynomial
                                           ,Evaluation
                                           ,polynomial
                                           ,evaluation
                                           ,differentiate
                                           ,integrate
                                           ,evaluate
                                           ,add
                                           ,substract
                                           ,mult
                                           ,variables
                                          ) where

import qualified Data.Map as M
import Data.List (delete, nub) 
import Data.Maybe

-- | Representation of a multivariate polynomial.
type Polynomial = M.Map String Double


polynomial :: [(String, Double)] -> Polynomial
polynomial = M.fromList 


-- | Type alias to evaluate a 'Polynomial'. See the 'evaluate' function for an example of usage.
type Evaluation = M.Map Char Double


evaluation :: [(Char, Double)] -> Evaluation
evaluation = M.fromList


-- count the occurences of a monomial in a term
occ :: Char -> String -> Double 
occ _ [] = 0.0
occ c (x:xs) = let n = if c == x then 1.0 else 0.0 in n + occ c xs


-- | The 'differentiate' function calculates the derivative of a 'Polynomial' with respect to a given variable.
--
-- > let p = polynomial [("",1.0), ("x",2.0), ("y",3.0), ("xy",4.0), ("xxy",5.0)] :: Polynomial
-- > differentiate p 'x' == polynomial [("",2.0),("xy",10.0),("y",4.0)]
-- > differentiate p 'y' == polynomial [("",3.0),("x",4.0),("xx",5.0)]
differentiate :: Polynomial -> Char -> Polynomial
differentiate p c = M.mapKeys (delete c) $
                    M.mapWithKey (\ k v -> (occ c k) * v) $
                    M.filterWithKey (\ k _ -> c `elem` k) p


-- | The 'integrate' function calculates the integral of a 'Polynomial' with respect to a given variable.
--
-- > let p = polynomial [("",1.0), ("x",2.0), ("y",3.0), ("xy",4.0)]
-- > integrate p 'x' == polynomial [("x",1.0),("xx",1.0),("xxy",2.0),("xy",3.0)]
-- > integrate p 'y' == polynomial [("y",1.0),("yx",2.0),("yxy",2.0),("yy",1.5)]
integrate :: Polynomial -> Char -> Polynomial
integrate p c = M.mapKeys (c:) $ M.mapWithKey (\ k v -> v / (occ c k + 1.0)) p


-- | The 'evaluate' function evaluates a 'Polynomial' at a point defined through the 'Evaluation' type alias:
--
-- > let p = polynomial [("",1.0), ("x",2.0), ("y",3.0), ("xy",4.0), ("xxy",5.0)]
-- > let e = evaluation [('x',2.0), ('y',1.0)]
-- > evaluate p e == 36.0
evaluate :: Polynomial -> Evaluation -> Double
evaluate p e = M.foldl (+) 0.0 $ evals p e

evals :: Polynomial -> Evaluation -> Polynomial
evals p e = evals' p e $ M.keys e
    where
      evals' q _ [] = q
      evals' q ev (c:cs) = evals' (eval q ev c) ev cs                 

eval :: Polynomial -> Evaluation -> Char -> Polynomial
eval p e c = M.mapWithKey f p
    where
      f k v = v * (x ** (occ c k))
      x = fromJust $ M.lookup c e

instance Num Polynomial where
    p + q = add p q
    p * q = mult p q
    negate = M.map negate
    signum = M.map signum 
    fromInteger = undefined         
    abs = M.map abs

-- | Addition of two 'Polynomial's.
--
-- > let p = polynomial [("",1.0), ("x",2.0), ("y",3.0)]
-- > let q = polynomial [("x",2.0), ("xy",4.0)]
-- > add p q == polynomial [("",1.0),("x",4.0),("xy",4.0),("y",3.0)]
add :: Polynomial -> Polynomial -> Polynomial
add p q = M.unionWith (+) p q

-- | Substraction of two 'Polynomial's.
--
-- > let p = polynomial [("",1.0), ("x",2.0), ("y",3.0)] 
-- > let q = polynomial [("x",2.0), ("xy",4.0)]
-- > substract p q == polynomial [("",1.0),("x",0.0),("xy",4.0),("y",3.0)]
substract :: Polynomial -> Polynomial -> Polynomial
substract p q = add p $ M.map negate q

-- | Multiplication of two 'Polynomial's.
--
-- > let p = polynomial [("",1.0), ("x",2.0), ("y",3.0)]
-- > let q = polynomial [("x",2.0), ("xy",4.0)]
-- > mult p q == polynomial [("x",2.0),("xx",4.0),("xxy",8.0),("xy",4.0),("yx",6.0),("yxy",12.0)]
mult :: Polynomial -> Polynomial -> Polynomial
mult p q = mult' p q $ M.keys p
    where
      mult' _ _ [] = polynomial []
      mult' p' q' (key:keys) = add (M.map (* (p' M.! key)) $ M.mapKeys (key ++) q') (mult' p' q' keys)

-- | The 'variables' function returns the list of variables used to 
-- define a 'Polynomial'.
--
-- > let p = polynomial [("",1.0),("x",4.0),("xy",4.0),("y",3.0)]
-- > variables p == "xy"
variables :: Polynomial -> [Char]
variables = nub . concat . M.keys