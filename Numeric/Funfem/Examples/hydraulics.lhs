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
>  let boundaries = boundariesFromInput i

>  print elements

Funfem provides a wrapper for simple cases like this one. It just
needs the function to build the elementary stiffnesses, in our case
the function elementaryStiffness defined above, the elements and the
boundary conditions (currently only Dirichlet boundary conditions are
supported).

>  let (globStiff,rhs) = buildSystem elementaryStiffness elements boundaries

Solve the system using LU decomposition with forward and backward substitutions:

>  let x = luSolve globStiff rhs
>  print x

And that's it! Ok, now we need to pretty print the output, etc, but at
least, in 16 lines of code we have a simple case scenario.

The following is for testing purposes only. You can have a look at the
documentation and/or the source code for more information.

>  let nodes = nodesFromInput i
>  let s0 = toGlobal elementaryStiffness elements
>  let rhs' = buildRHS boundaries (length nodes)
>  let s = applyBoundaryConditions boundaries s0
>  let stiffness = toMatrix s
>  print stiffness

>  let x' = luSolve stiffness rhs'
>  print x'       




