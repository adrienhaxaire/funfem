{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Geometry where

import GHC.Generics
import Control.Applicative
import Control.Monad
import Data.Binary as B
import Data.Aeson as A

data Point = Point1D {x :: Double}
           | Point2D {x :: Double, y :: Double}
           | Point3D {x :: Double, y :: Double, z :: Double}
             deriving (Eq, Ord, Show, Generic)

                      
data Node = Node { nodeNumber :: Int
                 , nodePoint :: Point }
          deriving (Eq, Ord, Show, Generic)

