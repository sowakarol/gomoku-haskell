module Point where

import Color

data Point = Point{color::Color, position::(Int,Int)}

instance Show Point where
    show (Point color _) = show color

instance Eq Point where
    (Point color1 (a,b)) == (Point color2 (c,d)) = (a == c && b == d && color1 == color2)
