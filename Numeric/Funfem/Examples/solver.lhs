This file is intended two show how to use the conjugate gradient solver
present in the Solver module.
The following instruction will allow us to load this module in GHCi
which is quite convenient.

> module Main where

We need the Vector module to build the system to be solved,
using the Matrix and Vector data types.

> import Numeric.Funfem.Vector

We obviously need the Solver module. It only contains the conjugate gradient
at the moment, which isn't even preconditionned!

> import Numeric.Funfem.Solver

Usual main function declaration.

> main :: IO()
> main = do

Here we build a matrix, ie a typed list of Vector. The function
fromVectors can be used as well.  

>   let m = Matrix [Vector [1,2], Vector [3,4]]

The cg function does not make a fist guess, so we initialize it with zeros. 
This will change in future versions.

>   let x = Vector [0,0]

Put some values in the RHS vector,

>   let v = Vector [3,80]

And finally do the effective calculation and display the result.

>   let r = cg m x v
>   print r  