This file is intended two show how to use the conjugate gradient solver
present in the Solver module.
The following instruction will allow us to load this module in GHCi
which is quite convenient.

> module Main where

I will write a proper explanation soon :)

> import Numeric.Funfem

> main :: IO ()
> main = do

>   let m = matrix [[1.0,2.0],[3.0,4.0]]

>   let v = vector [3.0,80.0]

>   let r = cg m v
        
The result should be 74 and -35.5                     
        
>   print r  