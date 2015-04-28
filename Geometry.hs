{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Geometry where

import GHC.Generics
import Data.Aeson

data Point = Point1D {x :: Double}
           | Point2D {x :: Double, y :: Double}
           | Point3D {x :: Double, y :: Double, z :: Double}
             deriving (Eq, Ord, Show, Generic)

instance ToJSON Point
instance FromJSON Point
                     
data Node = Node { nodeNumber :: Int
                 , nodePoint :: Point}
            deriving (Eq, Ord, Show, Generic)

instance ToJSON Node
instance FromJSON Node




