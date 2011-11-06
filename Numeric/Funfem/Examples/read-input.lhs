

> module Main where

> import Numeric.Funfem

> main :: IO()
> main = do
>   f <- readFile "read-input.json"
>   let i = inputFromString f
>   let elements = elementsFromInput i    
>   let materials = materialsFromInput i
>   let boundaries = boundariesFromInput i
>   print elements
>   print materials
>   print boundaries

