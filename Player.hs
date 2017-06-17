module Player where

import Board
import Color
import Point

data Player = Player{color::Color}

-- isInteger s = case reads s :: [(Integer, String)] of
--   [(_, "")] -> True
--   _         -> False

addPoint :: Player -> (Int,Int) -> Board -> Board
addPoint (Player c) (x,y) (Board a b points) = Board a b pointsChanged
    where
        (h,pointRow:t) = splitAt (y - 1) points
        (head,_:tail) = splitAt (x-1) pointRow
        pointsChanged = h ++ [head ++ ((Point c (x,y)) : tail)] ++ t


-- turn::Player -> Board -> String -> Board
-- turn player board =
--     show "Write x y to make a move"
--     let line <- getLine
--     let a:_:b <- line
--     if isInteger a && isInteger b then Player.addPoint else do
--         show "WRONG FORMAT!"
--         turn player board


-- addPoint:: Player -> Int -> Int -> Board -> Board
-- addPoint (Player color) x y board=
--     if (checkIfPointIsInBoard (Point color (x,y)) board) && (checkIfPointIsEmpty (Point color (x,y)) board) then Board.addPoint else do
--         putStrLn "Wrong coordinates!"
--         turn (Player color) board
    



 --   | (checkIfPointIsInBoard (Point c (x,y)) (Board a b points) && checkIfPointIsEmpty (Point c (x,y)) (Board a b points) ) = 
  --      addPoint (Point c (x,y)) (Board a b points)
  --  | otherwise = (Board a b points)