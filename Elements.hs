---------------------------------------------------------------------------------- 
-- |
-- Module : Elements
-- Copyright : (c) Adrien Haxaire 2011
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@funfem.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--
-- Defines Node, Element and Material data
--

module Elements where

import Text.JSON

type Coordinates = (Double, Double)
type Number = Int
type Name = String
type Value = Double

-- Nodes
data Node = Node Coordinates Number
          deriving (Eq, Ord, Show)

getNodeNumber :: Node -> Number
getNodeNumber (Node _ number) = number

getNodeCoordinates :: Node -> Coordinates
getNodeCoordinates (Node coordinates _ ) = coordinates 

instance JSON Node where
  readJSON object = do 
    obj <- readJSON object
    coords <- valFromObj "coordinates" obj
    number <- valFromObj "number" obj
    return (Node coords number)
  showJSON (Node coords number) = makeObj [("coordinates",showJSON coords)
                                          ,("number", showJSON number)]

-- Elements
-- | [Number] is the list of Node numbers, Number is the Element number and  
-- Name refers to the name of the Element material
data Element = Element [Number] Number Name
             deriving (Eq, Ord, Show)

getElementNodes :: Element -> [Number]
getElementNodes (Element nodes _ _) = nodes

getElementNumber :: Element -> Number
getElementNumber (Element _ number _) = number

getElementMaterial :: Element -> Name
getElementMaterial (Element _ _ name) = name


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
data Property = Property Name Value
              deriving (Eq, Ord, Show)

instance JSON Property where
  readJSON object = do
    obj <- readJSON object
    name <- valFromObj "name" obj
    value <- valFromObj "value" obj
    return (Property name value)
  showJSON (Property name value) = makeObj [("name", showJSON name)
                                            ,("value", showJSON value)]

getPropertyValue :: Property -> Value
getPropertyValue (Property _ value) = value

getPropertyName :: Property -> Name
getPropertyName (Property name _) = name


data Material = Material Name [Property] Number
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

getMaterialName :: Material -> Name
getMaterialName (Material name _ _) = name

getMaterialProperties :: Material -> [Property]
getMaterialProperties (Material _ properties _) = properties

getMaterialNumber :: Material -> Number
getMaterialNumber (Material _ _ number) = number

getMaterialFromName :: Name -> [Material] -> Material
getMaterialFromName _ [] = Material "Null" [] 0
getMaterialFromName n (m:ms) = if (getMaterialName m == n) 
                               then m
                               else getMaterialFromName n ms

-- Boundary conditions
-- | [Number] is the list of nodes affected by the boundary conditions
-- i.e. no Neumann BC handled yet

data BoundaryCondition = BoundaryCondition Name [Number] Value
                         deriving (Eq, Ord, Show)
                                  
instance JSON BoundaryCondition where
  readJSON object = do
    obj <- readJSON object
    name <- valFromObj "name" obj
    nodes <- valFromObj "nodes" obj
    value <- valFromObj "value" obj
    return (BoundaryCondition name nodes value)
  showJSON (BoundaryCondition name nodes value) = makeObj [("name", showJSON name)
                                                          ,("nodes", showJSON nodes)
                                                          ,("value", showJSON value)]
getBCName :: BoundaryCondition -> Name
getBCName (BoundaryCondition name _ _) = name

getBCNodes :: BoundaryCondition -> [Number]
getBCNodes (BoundaryCondition _ numbers _) = numbers

getBCValue :: BoundaryCondition -> Value
getBCValue (BoundaryCondition _ _ value) = value