module Shapes (
    Point(..),
    Shape(..),
    area,
    nudge,
    baseCircle,
    baseRect
) where

data Shape' = Circle' Float Float Float | Rectangle' Float Float Float Float
    deriving (Show)

area' :: Shape' -> Float
area' (Circle' _ _ radius) = pi * (radius ^ 2)
area' (Rectangle' x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
{-
 - *Main> area $ Circle' 1 1 10
 - 314.15927
 - *Main> area $ Rectangle' 0 0 10 10
 - 100.0
 -
 - Data constructor are also functions,
 - so we can do all function tricks:
 - *Main> map (Circle' 0 0) [1..3]
 - [Circle' 0.0 0.0 1.0,Circle' 0.0 0.0 2.0,Circle' 0.0 0.0 3.0]
 -}


data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ radius) = pi * (radius ^ 2)
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
{-
 - *Main> area $ Rectangle (Point 0 0) (Point 100 100)
 - 10000.0
 - *Main> area (Circle (Point 10 1) 20)
 - 1256.6371
 -}

{- move shapes -}
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) radius) dx dy = Circle (Point (x + dx) (y + dy)) radius
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy = Rectangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy)) 

{- create shapes in (0, 0) point -}
baseCircle :: Float -> Shape
baseCircle radius = Circle (Point 0 0) radius

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

{-
 - *Main> nudge (baseRect 20 10) 4 1
 - Rectangle (Point 4.0 1.0) (Point 24.0 11.0)
 -}
