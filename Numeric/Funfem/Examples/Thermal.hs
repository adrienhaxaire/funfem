import Numeric.Funfem

n1, n2, n3, n4 :: Node
n1 = Node 1 [0.0, 0.0]
n2 = Node 2 [1.0, 0.0]
n3 = Node 3 [1.0, 1.0]
n4 = Node 4 [0.0, 1.0]

mat :: Material
mat = mkMaterial [("conductivity", 3.0)]

conductivity :: Tri3 -> Double
conductivity t = fromJust $ property "conductivity" t

el1, el2 :: Tri3
el1 = Tri3 [n1, n2, n3] mat
el2 = Tri3 [n3, n4, n1] mat

solve :: IO ()
solve = do   
  let a = matrix [[1.0,2.0],[3.0,4.0]]
  let v = vector [3.0,80.0]
  let res = cg a v
  print res

bc :: BoundaryCondition
bc = Dirichlet [(1,10.0)] 

lhs :: Tensor Double
lhs = assemble [el1] $ conductivity el1

main = print $ cg lhs rhs 

