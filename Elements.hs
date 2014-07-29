{-# LANGUAGE DeriveGeneric #-}

module Elements where

import GHC.Generics
import Data.Binary

data Node = Node {nodeNumber :: Int, coords :: [Double]}
            deriving (Eq, Ord, Show)

-- | Based on the numbering from Gmsh. Few of them are officially supported; but
-- at least 15 were needed to be able to read one-node points.
data ElementType = Unsupported
                 | Line2
                 | Tri3
                 | Qua4
                 | Tet4
                 | Hex8
                 | Prism6
                 | Pyr5
                 | Line3
                 | Tri6
                 | Qua9
                 | Tet10
                 | Hex27
                 | Prism18
                 | Pyr14
                 | Point1
                 | Qua8
                 | Hex20
                 | Prism15
                 | Pyr13
                 deriving (Eq, Ord, Show, Enum, Generic)

instance Binary ElementType


numNodes :: ElementType -> Int
numNodes et = case et of
                Unsupported -> 0
                Line2 -> 2
                Tri3 -> 3
                Qua4 -> 4
                Tet4 -> 4
                Hex8 -> 8
                Prism6 -> 6
                Pyr5 -> 5
                Line3 -> 3
                Tri6 -> 6
                Qua9 -> 9
                Tet10 -> 10
                Hex27 -> 27
                Prism18 -> 18
                Pyr14 -> 14
                Point1 -> 1
                Qua8 -> 8
                Hex20 -> 20
                Prism15 -> 15
                Pyr13 -> 13

data Element = Element { elType :: ElementType
                       , elNum :: Int
                       , elNodes :: [Int]} 
          deriving (Eq, Ord, Show, Generic)

instance Binary Element







