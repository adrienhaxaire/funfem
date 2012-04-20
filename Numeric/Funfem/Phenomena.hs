---------------------------------------------------------------------------------- 
-- |
-- Module : Numeric.Funfem.Phenomena
-- Copyright : (c) Adrien Haxaire 2011-2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
--

module Numeric.Funfem.Phenomena (
                                 Phenomenon
                                 ,flux
                                 ,storage
                                 ,source
                                ) where

import Numeric.Funfem.Algebra.Tensor as T
import Numeric.Funfem.Elements

type Phenomenon = Tensor Shape

-- | Matrix resulting from the product between the transposed of the
-- gradient, a constant, and the gradient.  This function is quite handy when
-- dealing with fluxes, as in Fourier's law for example.
flux :: Element a => Double -> a -> Phenomenon
flux x el = let m = matrix $ grad el
            in T.transpose m * fmap (* constShape x) m

-- | Matrix resulting from the product between the transposed of the
-- shape functions, a constant, and the shape functions. Useful for storage 
-- terms, like heat capacity for example.
storage :: Element a => Double -> a -> Phenomenon
storage x el = let v = vector $ shapes el
               in T.transpose v * fmap (* constShape x) v

-- | Matrix resulting from the product between the transposed of the
-- shape functions and a constant. Useful for source terms,
-- like a volumetric heat source for example.
source :: Element a => Double -> a -> Phenomenon
source x el = fmap (* constShape x) $ T.transpose (vector $ shapes el)
