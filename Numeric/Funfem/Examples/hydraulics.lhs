This file is an example on how to use funfem in order to build a
simple calculation.

For ease of use, you can declare your main function as part of a Main
module. This will allow us to load it in GHCi if we want to.

> module Main where

Let's import the whole module.

> import Numeric.Funfem

Using the tri3' function defined in the Numeric.Funfem.ShapeFunctions
module (the derivative of the shape function), we can create a
function to build the elementary stiffness matrix for each element:

> elementaryStiffness :: Element -> Matrix
> elementaryStiffness el = (transpose $ tri3' el) * (permeability `multSM` (tri3' el))
>   where
>     permeability = matPropertyFromName mat "permeability"
>     mat = elemMaterial el

and declare the main function as usual.

> main :: IO()
> main = do
  
Now let's load some data. Funfem currently supports only JSON for its
built-in data type. It will allow other kind of more friendly inputs,
like Gmsh for example.  

>  f <- readFile "hydraulics.json"

Here we extract the data contained in the input file. Notice the use
of the helper functions:

>  let i = inputFromString f

>  let elements = elementsFromInput i    
>  print elements       
       
>  let boundaries = boundariesFromInput i
>  let nodes = nodesFromInput i

>  let s = toGlobal elementaryStiffness elements

>  let rhs = buildRHS boundaries (length nodes)
       
>  let s' = setStiffnessRow 1 0.0 s
>  let s = setStiffness (1,1) 1.0 s'     
  
>  let stiffness = toMatrix $ adaptGlobalToRHS rhs s
>  print stiffness

>  let x0 = genVector (length nodes) 0.0

  let x = cg stiffness x0 rhs
  print x       

As this library is still work in progress, our example stops here at
the moment. It will be wxtended as soon as the developments in the
library allow it.




-- only here for development
