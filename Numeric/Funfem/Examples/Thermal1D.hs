import Numeric.Funfem

n1, n2, n3 :: Node
n1 = Node 1 [0.0]
n2 = Node 2 [1.0]
n3 = Node 3 [2.0]

mat :: Material
mat = mkMaterial [("conductivity", 2.0)]

el1, el2 :: Lin2
el1 = Lin2 [n1,n2] mat
el2 = Lin2 [n2,n3] mat

conduction :: Element a -> Phenomenon
conduction



main = do   
  let a = matrix [[1.0,2.0],[3.0,4.0]]
  let v = vector [3.0,80.0]
  let res = cg a v
  print res

