This file is intended two show how to use the conjugate gradient solver
present in the Solver module.
The following instruction will allow us to load this module in GHCi
which is quite convenient.

> module Main where

Funfem uses the excellent Data.Vector package. It is necessary to deal
with vectors and matrices in Funfem (at the moment).

> import qualified Data.Vector as V

We obviously need the Solver module. It only contains the conjugate gradient
at the moment, which isn't even preconditionned!

> import Numeric.Funfem.Algebra.Solver.CG

Usual main function declaration.

> main :: IO()
> main = do

There is no easy way (yet) to create a matrix, which is a vector of vectors.
  
>   let m = V.fromList [V.fromList [1.0,2.0], V.fromList [3.0,4.0]]

Put some values in the RHS vector,

>   let v = V.fromList [3.0,80.0]

And finally do the effective calculation and display the result.

>   let r = cg m v
        
The result should be 74 and 35.5                     
        
>   print r  