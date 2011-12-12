---------------------------------------------------------------------------------- 
-- |
-- Module : Elements
-- Copyright : (c) Adrien Haxaire 2011
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
----------------------------------------------------------------------------------
--
-- Defines Node, Element, Material and BoundaryCondition data

module Numeric.Funfem.Elements where

import Text.JSON
type Coordinates = (Double, Double)
type Number = Int
type Name = String
type Value = Double

-- Nodes
data Node = Node {nodeCoordinates :: Coordinates 
                 ,nodeNumber :: Number}
          deriving (Eq, Ord, Show)

instance JSON Node where
  readJSON object = do 
    obj <- readJSON object
    coords <- valFromObj "coordinates" obj
    number <- valFromObj "number" obj
    return (Node coords number)
  showJSON (Node coords number) = makeObj [("coordinates",showJSON coords)
                                          ,("number", showJSON number)]

-- Elements
data Element = Element {elemNodes :: [Node] 
                       ,elemNumber :: Number 
                       ,elemMaterial :: Material}
             deriving (Eq, Ord, Show)

instance JSON Element where
  readJSON object = do
    obj <- readJSON object
    nodes <- valFromObj "nodes" obj
    number <- valFromObj "number" obj
    material <- valFromObj "material" obj
    return (Element nodes number material)
  showJSON (Element nodes number material) = makeObj [("nodes", showJSON nodes)
                                            ,("number", showJSON number)
                                            ,("material", showJSON material)]

-- Materials

-- | Property data type to simplify creation of a Material data type
data Property = Property {propName :: Name
                         ,propValue :: Value}
              deriving (Eq, Ord, Show)

instance JSON Property where
  readJSON object = do
    obj <- readJSON object
    name <- valFromObj "name" obj
    value <- valFromObj "value" obj
    return (Property name value)
  showJSON (Property name value) = makeObj [("name", showJSON name)
                                            ,("value", showJSON value)]

data Material = Material {matName :: Name
                         ,matProperties :: [Property]
                         ,matNumber :: Number}
              deriving (Eq, Ord, Show)
                       
instance JSON Material where
  readJSON object = do
    obj <- readJSON object
    name  <- valFromObj "name" obj
    properties <- valFromObj "properties" obj
    number <- valFromObj "number" obj
    return (Material name properties number)
  showJSON (Material name properties number) = makeObj [("name", showJSON name)
                                                       ,("properties", showJSON properties)
                                                       ,("number", showJSON number)]

matFromName :: Name -> [Material] -> Material
matFromName _ [] = Material "Null" [] 0        -- should handle it in a better way
matFromName n (m:ms) = if (matName m == n) 
                               then m
                               else matFromName n ms

matPropertyFromName :: Material -> String -> Double
matPropertyFromName mat name = propValue $ head property
  where property = filter (\n -> (propName n) == name) (matProperties mat)


