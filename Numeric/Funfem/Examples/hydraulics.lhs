This file is an example on how to use funfem in order to build a
simple calculation.

For ease of use, you can declare your main function as part of a Main
module. This will allow us to load it in GHCi if we want to.

> module Main where

Let's import the whole module,

> import Numeric.Funfem

and declare the main function as usual.

> main :: IO()
> main = do
  
Now let's load some data. Funfem currently supports only JSON for its
built-in data type. It will allow other kind of more friendly inputs,
like Gmsh for example.  

>  f <- readFile "read-input.json"

Here we extract the data contained in the input file. Notice the use
of the helper functions:

>  let i = inputFromString f
>  let elements = elementsFromInput i    
>  let materials = materialsFromInput i
>  let boundaries = boundariesFromInput i
>  print elements

As this library is still work in progress, our example stops here at
the moment. It will be wxtended as soon as the developments in the
library allow it.