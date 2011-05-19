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
type Type = String


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
-- | [Number] is the list of Node numbers
data Element = Element [Number] Number
             deriving (Eq, Ord, Show)

getElementNodes :: Element -> [Number]
getElementNodes (Element nodes _) = nodes

getElementNumber :: Element -> Number
getElementNumber (Element _ number) = number


instance JSON Element where
  readJSON object = do
    obj <- readJSON object
    nodes <- valFromObj "nodes" obj
    number <- valFromObj "number" obj
    return (Element nodes number)
  showJSON (Element nodes number) = makeObj [("nodes", showJSON nodes)
                                            ,("number", showJSON number)]


-- Materials
type Name = String
type Value = Double

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


