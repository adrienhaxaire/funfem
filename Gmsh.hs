module Gmsh where

import System.Environment
import Text.ParserCombinators.Parsec
--import qualified Data.Text as T
--import Data.Text.Lazy.IO as TIO

import Elements

data MeshFormat = MeshFormat {versionNumber :: Double
                             , fileType :: Int
                             , dataSize :: Int}
                  deriving (Show)

meshFormatParser :: Parser MeshFormat
meshFormatParser = do
  string "$MeshFormat"
  newline
  v <- anyChar `manyTill` space
  spaces
  f <- anyChar `manyTill` space
  spaces
  d <- anyChar `manyTill` newline
  string "$EndMeshFormat"
  newline
  return $ MeshFormat (read v) (read f) (read d)

nodesParser :: Parser [Node]
nodesParser = do
  string "$Nodes"
  newline
  numNodes <- digit `manyTill` newline
  nodes <- readNodes (read numNodes)
  string "$EndNodes"
  return nodes

readNodes :: Int -> Parser [Node]
readNodes n = go n []
    where
      go 0 ns = return $ reverse ns
      go n ns = do
        num <- digit `manyTill` space
        spaces
        x <- anyChar `manyTill` space
        spaces
        y <- anyChar `manyTill` space
        spaces
        z <- anyChar `manyTill` space
        let node = Node (read num) [(read x), (read y), (read z)]
        go (n - 1) (node : ns)

adaptToDim :: [Node] -> [Node]
adaptToDim ns = if all canBeReduced ns 
                then map reduce ns
                else ns
                    where
                      canBeReduced (Node _ cs) = last cs == 0.0
                      reduce (Node n cs) = Node n (init cs)

gmshParser :: Parser [Node]
gmshParser = do
  meshFormat <- meshFormatParser -- do sth with it
  nodes <- nodesParser
  return $ adaptToDim nodes

importGmsh :: String -> [Node]
importGmsh text = case parse gmshParser "oops" text of
                    Right r -> r
                    Left _ -> []


-- let l = "$MeshFormat\n2.2 0 8\n$EndMeshFormat"
-- parse meshFormatParser "oops" l



