{-# LANGUAGE BangPatterns #-}
module Vector where

import Prelude as P hiding (foldr, (++), concat)

import GHC.Conc (numCapabilities)
import Control.Monad.Par
import Data.Monoid (Monoid(..))
import qualified Data.Foldable as F

data Vector a = Vector { lengthV :: !Int
                       , elemsV :: [[a]]}
                deriving (Eq, Ord, Show)

instance NFData (Vector a)

instance Functor Vector where
    fmap f (Vector n vs) = Vector n (map (map f) vs)

instance Monoid (Vector a) where
    mempty = empty
    mappend = (++)
    mconcat = concat

-- TODO !
--instance F.Foldable Vector where
--    foldMap f (Vector n xs) = 
--    foldMap f v = F.foldMap f (F.concat (elems v)) -- bad for //


splitElems :: [a] -> [[a]]
splitElems as = reverse $ go numCapabilities as []
    where
      slice = P.length as `div` numCapabilities
      go 1 xs ys = (xs:ys)
      go i xs ys = go (i - 1) (drop slice xs) (take slice xs : ys)

empty :: Vector a
empty = Vector 0 []

(++) :: Vector a -> Vector a -> Vector a
(++) (Vector i us) (Vector j vs) = Vector (i + j) xs
           where
             xs = splitElems $ F.concat [F.concat us, F.concat vs]

concat :: [Vector a] -> Vector a
concat vs = Vector len (splitElems es)
    where
      len = F.sum $ fmap lengthV vs
      es = F.concat $ F.concat $ map elemsV vs


vector :: [a] -> Vector a
vector [] = empty
vector !as = Vector (P.length as) (splitElems as)



