{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Gmsh (importGmsh) where

import Text.ParserCombinators.Parsec

import Elements

int :: Parser Int
int = do
  i <- many1 digit
  return (read i)

double :: Parser Double
double = do
  sign <- optionMaybe (char '-')
  ds <- many1 (digit <|> char '.')
  return $ case sign of
             Nothing -> read ds
             Just s -> read (s:ds)

-- may be used later on for compatibility, etc
data MeshFormat = MeshFormat Double Int Int deriving (Show)

meshFormatParser :: Parser MeshFormat
meshFormatParser = do
  string "$MeshFormat"
  newline
  versionNumber <- double 
  space
  fileType <- int
  space
  dataSize <- int
  newline
  string "$EndMeshFormat"
  return $ MeshFormat versionNumber fileType dataSize

node :: Parser Node
node = do
  num <- int
  coors <- count 3 $ space >> double
  return $ Node num coors

nodesParser :: Parser [Node]
nodesParser = do
  string "$Nodes"
  newline
  num <- int
  ns <- count num $ newline >> node
  newline
  string "$EndNodes"
  return ns

adaptToDim :: [Node] -> [Node]
adaptToDim ns = if all canBeReduced ns 
                then map reduceDim ns
                else ns
                    where
                      canBeReduced (Node _ cs) = last cs == 0.0
                      reduceDim (Node n cs) = Node n (init cs)

tags :: Parser [Int]
tags = do 
  num <- int
  count num $ space >> int

element :: Parser Element
element = do 
  num <- int
  space
  typ <- int
  space
  tags
  conn <- count (numNodes $ toEnum typ) $ space >> int
  return $ Element (toEnum typ) num conn

elementsParser :: Parser [Element]
elementsParser = do
  string "$Elements"
  newline
  num <- int
  elements <- count num $ newline >> element
  newline
  string "$EndElements"
  return elements

gmshParser :: Parser ([Node], [Element])
gmshParser = do
  _ <- meshFormatParser -- do sth with it
  newline
  nodes <- nodesParser
  newline
  elements <- elementsParser
  return (adaptToDim nodes, elements)

importGmsh :: String -> ([Node], [Element])
importGmsh text = case parse gmshParser "oops" text of
                    Right r -> r
                    Left _ -> ([], [])


