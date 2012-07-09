{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
---------------------------------------------------------------------------------- 
-- |
-- Module : Numeric.Funfem.Algebra.Polynomials
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@haxaire.org>
-- Stability : experimental
-- Portabilty : not tested
-- 
-- Multivariate polynomials. Defined to represent shape functions, allowing
-- their exact multiplication, addition and integration. 
-- 
-- A barebone Num instance is provided, but the functions 'add', 'substract'
-- and 'mult' are provided for comodity.
--
module Numeric.Funfem.Algebra.Polynomials (
                                           Polynomial
                                           ,Evaluation
                                           ,mkPoly
                                           ,mkEval
                                           ,differentiate
                                           ,integrate
                                           ,integrateBetween
                                           ,eval
                                           ,evaluate
                                           ,inner
                                           ,variables
                                           ,isConstant
                                          ) where

import qualified Data.Map as M
import Data.List (delete, nub, transpose, foldl') 
import Data.Maybe (fromJust)

-- | Representation of a multivariate polynomial.
type Polynomial = M.Map String Double

-- | Helper function to create a 'Polynomial' from an association list.
mkPoly :: [(String, Double)] -> Polynomial
mkPoly = M.fromList 

-- | Type alias to evaluate a 'Polynomial'. See the 'evaluate' function for an example of usage.
type Evaluation = M.Map Char Double

-- | Helper function to create an 'Evaluation' from an association list.
mkEval :: [(Char, Double)] -> Evaluation
mkEval = M.fromList

-- count the occurences of a monomial in a term
occ :: Char -> String -> Double 
occ _ [] = 0.0
occ c (x:xs) = let n = if c == x then 1.0 else 0.0 in n + occ c xs

-- | The 'differentiate' function calculates the derivative of a 'Polynomial'
-- with respect to a given variable.
-- 
-- >>> let p = mkPoly [("",1.0), ("x",2.0), ("y",3.0), ("xy",4.0), ("xxy",5.0)]
-- >>> differentiate p 'x' == mkPoly [("",2.0),("xy",10.0),("y",4.0)]
-- True
-- >>> differentiate p 'y' == mkPoly [("",3.0),("x",4.0),("xx",5.0)]
-- True
differentiate :: Polynomial -> Char -> Polynomial
differentiate p c = M.mapKeys (delete c) $
                    M.mapWithKey (\ k v -> (occ c k) * v) $
                    M.filterWithKey (\ k _ -> c `elem` k) p

-- | The 'integrate' function calculates the integral of a 'Polynomial' with respect to a given variable.
--
-- >>> let p = mkPoly [("",1.0), ("x",2.0), ("y",3.0), ("xy",4.0)]
-- >>> integrate p 'x' == mkPoly [("x",1.0),("xx",1.0),("xxy",2.0),("xy",3.0)]
-- True
-- >>> integrate p 'y' == mkPoly [("y",1.0),("yx",2.0),("yxy",2.0),("yy",1.5)]
-- True
integrate :: Polynomial -> Char -> Polynomial
integrate p c = M.mapKeys (c:) $ M.mapWithKey (\ k v -> v / (occ c k + 1.0)) p

-- | The 'integrateBetween' function calculates the integral of a 'Polynomial'
-- according to a variable between the specified lower and upper bounds.
integrateBetween :: Polynomial -> Char -> Double -> Double -> Polynomial
integrateBetween p c a b = eval prim eb c - eval prim ea c
    where
      prim = integrate p c
      ea = mkEval [(c,a)]
      eb = mkEval [(c,b)]

-- | The 'evaluate' function evaluates a 'Polynomial' at a point defined through the 'Evaluation' type alias:
--
-- >>> let p = mkPoly [("",1.0), ("x",2.0), ("y",3.0), ("xy",4.0), ("xxy",5.0)]
-- >>> let e = mkEval [('x',2.0), ('y',1.0)]
-- >>> evaluate p e == 36.0
-- True
evaluate :: Polynomial -> Evaluation -> Double
evaluate p e = M.foldl (+) 0.0 $ evals p e

evals :: Polynomial -> Evaluation -> Polynomial
evals p e = evals' p e $ M.keys e
    where
      evals' q _ [] = q
      evals' q ev (k:ks) = evals' (eval q ev k) ev ks                 

-- | The 'eval' function partially 'evaluate's a 'Polynomial' for a given variable.
--
-- >>> let p = mkPoly [("",1.0), ("x",2.0), ("y",3.0), ("xy",4.0), ("xxy",5.0)]
-- >>> let e = mkEval [('x',2.0), ('y',1.0)]
-- >>> eval p e 'x' == mkPoly [("",5.0),("y",31.0)]
-- True
eval :: Polynomial -> Evaluation -> Char -> Polynomial
eval p e c = deleteVariable evaluated c keys
    where
      keys = M.keys p
      evaluated = M.mapWithKey f p
      f k v = v * (x ** (occ c k))
      x = fromJust $ M.lookup c e

deleteVariable :: Polynomial -> Char -> [String] -> Polynomial
deleteVariable p _ [] = p
deleteVariable p c (k:ks) = if c `elem` k
                            then deleteVariable p' c ks
                            else deleteVariable p c ks
                                where
                                  p' = M.delete k $ M.insertWith' (+) (wipe c k) (fromJust $ M.lookup k p) p

-- Remove all occurences of a character in a string
wipe :: Char -> String -> String
wipe c s = if c `elem` s then wipe c $ delete c s else s

instance Num Polynomial where
    p + q = add p q
    p * q = mult p q
    negate = M.map negate
    signum = M.map signum 
    fromInteger = undefined         
    abs = M.map abs

-- | Addition of two 'Polynomial's.
--
-- >>> let p = mkPoly [("",1.0), ("x",2.0), ("y",3.0)]
-- >>> let q = mkPoly [("x",2.0), ("xy",4.0)]
-- >>> add p q == mkPoly [("",1.0),("x",4.0),("xy",4.0),("y",3.0)]
-- True
add :: Polynomial -> Polynomial -> Polynomial
add p q = M.unionWith (+) p q

-- | Substraction of two 'Polynomial's.
--
-- >>> let p = mkPoly [("",1.0), ("x",2.0), ("y",3.0)] 
-- >>> let q = mkPoly [("x",2.0), ("xy",4.0)]
-- >>> substract p q == mkPoly [("",1.0),("x",0.0),("xy",-4.0),("y",3.0)]
-- True
substract :: Polynomial -> Polynomial -> Polynomial
substract p q = add p $ M.map negate q

-- | Multiplication of two 'Polynomial's.
--
-- >>> let p = mkPoly [("",1.0), ("x",2.0), ("y",3.0)]
-- >>> let q = mkPoly [("x",2.0), ("xy",4.0)]
-- >>> mult p q == mkPoly [("x",2.0),("xx",4.0),("xxy",8.0),("xy",4.0),("yx",6.0),("yxy",12.0)]
-- True
mult :: Polynomial -> Polynomial -> Polynomial
mult p q = mult' p q $ M.keys p
    where
      mult' _ _ [] = mkPoly []
      mult' p' q' (key:keys) = add (M.map (* (p' M.! key)) $ M.mapKeys (key ++) q') (mult' p' q' keys)

-- | The 'variables' function returns the list of variables used to 
-- define a 'Polynomial'.
--
-- >>> let p = mkPoly [("",1.0),("x",4.0),("xy",4.0),("y",3.0)]
-- >>> variables p == "xy"
-- True
variables :: Polynomial -> [Char]
variables = nub . concat . M.keys

-- | Checks if a 'Polynomial' is constant, i.e. does not contain
-- any variable.
--
-- >>> isConstant $ mkPoly []
-- True
-- >>> isConstant $ mkPoly [("",1.0)]
-- True
-- >>> isConstant $ mkPoly [("x",1.0)]
-- False
isConstant :: Polynomial -> Bool
isConstant p = (length $ variables p) == 0

-- | Equivalent of an inner product between two vectors
inner :: [Polynomial] -> [Polynomial] -> Polynomial
inner ps qs = foldl' add M.empty $ zipWith mult ps qs


