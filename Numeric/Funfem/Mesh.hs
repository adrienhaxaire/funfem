---------------------------------------------------------------------------------- 
-- |
-- Module : Numeric.Funfem.Mesh
-- Copyright : (c) Adrien Haxaire 2012
-- Licence : BSD3
--
-- Maintainer : Adrien Haxaire <adrien@haxaire.org>
-- Stability : experimental
-- Portabilty : not tested
--
----------------------------------------------------------------------------------
--

module Numeric.Funfem.Mesh (
                            Point
                           , Triangle
                           , triangle
                           , Mesh
                           , mesh
                           )where 

import qualified Data.List as L
import Data.Monoid
import Data.Foldable

mname :: String
mname = "Numeric.Funfem.Mesher."


type Point = (Double, Double) 

distance :: Point -> Point -> Double
distance p q = sqrt $ (fst q - fst p)**2 + (snd q - snd p)**2

vector :: Point -> Point -> Point
vector p q = (fst q - fst p, snd q - snd p)

cross :: Point -> Point -> Point -> Double
cross r c l = let (x1, y1) = vector c r
                  (x2, y2) = vector c l
              in x1 * y2 - x2 * y1

dot :: Point -> Point -> Point -> Double
dot r c l = let cr = vector c r
                cl = vector c l
            in (fst cr * fst cl) + (snd cl * snd cr)

angle :: Point -> Point -> Point -> Double
angle r c l = if r == l then 0 else
    let norms = distance c r * distance c l
        a = acos $ dot r c l / norms
    in case signum (cross r c l) of
         0.0 -> pi
         1.0 -> a
         -1.0 -> -a

positiveAngle :: Point -> Point -> Point -> Double
positiveAngle r c l = let a = angle r c l
                      in if cross r c l < 0 then 2 * pi - a
                         else a

validAngle :: Point -> Point -> Point -> Bool
validAngle r c l = let a = positiveAngle r c l
                   in (a >= (pi / 6)) && (a <= (2 * pi / 3))


type Length = Double

data Circle = Circle {center :: Point, radius :: Length} 
              deriving Show

circle :: Point -> Length -> Circle
circle p l = Circle p $ abs l

instance Eq Circle where
    (Circle c1 r1) == (Circle c2 r2) = c1 == c2 && abs(r1 - r2) < 1.0e-6

instance Ord Circle where
    compare c1 c2 = compare (radius c1) (radius c2)

inCircle :: Circle -> Point -> Bool
inCircle c p = distance (center c) p <= radius c

neighbours :: Point -> Length -> [Point] -> [Point]
neighbours p l = filter (inCircle $ circle p l)

data Triangle = Triangle Point Point Point 
                deriving (Show)

instance Eq Triangle where
    (Triangle p1 p2 p3) == (Triangle q1 q2 q3) =
        let ps = [p1, p2, p3]
        in q1 `L.elem` ps && q2 `L.elem` ps && q3 `L.elem` ps

instance Ord Triangle where
    compare t1 t2 = compare (area t1) (area t2)

trg :: Point -> Point -> Point -> Triangle
trg x y z 
    | x /= y && y /= z && x /= z = Triangle x y z
    | otherwise = error $ mname ++ "trg: Points should be different."

triangle :: Point -> Point -> Point -> Triangle
triangle = Triangle

area :: Triangle -> Double
area (Triangle a b c) = 0.5 * (abs $ cross a b c)

aspectRatio :: Triangle -> Double
aspectRatio (Triangle a b c) = 
    let distances = [distance a b, distance b c, distance c a]
        smallest = L.minimum distances
        longest = L.maximum distances
    in smallest / longest



data Mesh = Empty | Mesh [Triangle] 
              deriving (Eq, Ord, Show)

empty :: Mesh
empty = Empty

mesh :: [Triangle] -> Mesh
mesh = Mesh 

fromPoints :: [Point] -> Mesh
fromPoints = mesh . triangulate

instance Monoid Mesh where
    mempty = Empty
    mappend = append

append :: Mesh -> Mesh -> Mesh
append m Empty = m
append Empty m = m
append (Mesh m1) (Mesh m2) = Mesh (m1 ++ m2)

triangulate :: [Point] -> [Triangle]
triangulate [] = []
triangulate [_] = []
triangulate [_, _] = []
triangulate (p:q:r:rs) = undefined

quality :: Mesh -> Double
quality Empty = 0
quality (Mesh m) = 
    let ar = map aspectRatio m
        arsum = L.foldl1' (+) ar
        s = fromIntegral $ length m
    in arsum / s

{-
p1, p2, p3 :: Point
p1 = (0.0, 0.0)
p2 = (1.0, 0.0)
p3 = (1.0, 1.0)
p4 = (0.0, 1.0)
p5 = (-1.0, 1.0)
p6 = (-1.0, 0.0)
p7 = (-1.0, -1.0)
p8 = (0.0, -1.0)
p9 = (1.0, -1.0)

c3, c4, c5, c6 :: [Point]
c3 = [p1, p2, p3]
c4 = [p1, p2, p3, p4]
c5 = [p1, p2, p3, p4, p5]
c6 = [p1, p2, p3, p4, p5, p6]

t1, t2, t3 :: Triangle
t1 = trg p1 p2 p3
t2 = trg p3 p2 p4 
t3 = trg p2 p4 p5
-}