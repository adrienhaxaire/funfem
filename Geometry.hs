{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Geometry where

import GHC.Generics
import Data.Aeson

data Point = Point1D {xp :: Double}
           | Point2D {xp :: Double, yp :: Double}
           | Point3D {xp :: Double, yp :: Double, zp :: Double}
             deriving (Eq, Ord, Show, Generic)

instance ToJSON Point
instance FromJSON Point


data Node = Node { nodeNumber :: Int
                 , nodePoint :: Point}
            deriving (Eq, Ord, Show, Generic)

instance ToJSON Node
instance FromJSON Node

-- | The `x` coordinate of the `Node`.
xn :: Node -> Double
xn (Node _ p) = xp p

-- | The `y` coordinate of the given `Node`. An exception is thrown in the case of a dimension mismatch.
yn :: Node -> Double
yn (Node _ p) = yp p

-- | The `z` coordinate of the given `Node`. An exception is thrown in the case of a dimension mismatch.
zn :: Node -> Double
zn (Node _ p) = zp p





