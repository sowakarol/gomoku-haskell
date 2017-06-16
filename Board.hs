{-# LANGUAGE InstanceSigs #-}

module Board where

import Point
import Color
import Data.List


data Board = Board{width::Int, height::Int, points::[[Point]]}

instance Show Board where
    show (Board _ _ points) = intercalate "\n" $ map show points

generateEmptyBoard :: Int -> Int -> Board
generateEmptyBoard width height = Board width height points
    where
        points = [generateEmptyRow width x | x <- [1..height]]

generateEmptyRow :: Int -> Int -> [Point]
generateEmptyRow a rowNumber = if a > 0 then (generateEmptyRow (a - 1) rowNumber) ++ [Point Empty (a,rowNumber)] else []


checkIfPointIsInBoard:: Point -> Board -> Bool
checkIfPointIsInBoard (Point _ (x,y)) (Board a b _) = if (x <= a && y <= b) then True else False

addPointToBoard::Point -> Board -> Board
addPointToBoard (Point c (x,y)) (Board a b points)
    | (checkIfPointIsInBoard (Point c (x,y)) (Board a b points) && checkIfPointIsEmpty (Point c (x,y)) (Board a b points) ) = 
        addPoint (Point c (x,y)) (Board a b points)
    | otherwise = (Board a b points)

checkIfPointIsEmpty:: Point -> Board -> Bool
checkIfPointIsEmpty (Point c (x,y)) (Board a b points) = (Point c (x,y)) == point && (color == Empty)
    where 
    pointRow = points !! (y - 1)
    point = pointRow !! (x - 1)
    Point color _ = point

addPoint :: Point -> Board -> Board
addPoint (Point c (x,y)) (Board a b points) = Board a b pointsChanged
    where
        (h,pointRow:t) = splitAt (y - 1) points
        (head,_:tail) = splitAt (x-1) pointRow
        pointsChanged = h ++ [head ++ ((Point c (x,y)) : tail)] ++ t
