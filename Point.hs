module Point where

import Color

data Point = Point{color::Color, position::(Int,Int)}

instance Show Point where
    show (Point color _) = show color

instance Eq Point where
    (Point _ (a,b)) == (Point _ (c,d)) = (a == c && b == d)
