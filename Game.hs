module Game where

import Data.List

import Board
import Player
import Point
import Color


data Game = Game{numberOfTurns::Int, playerA::Player, playerB::Player, board::Board}

--play, validateInputFromUser TODO
-- play::Game -> IO()
-- play (Game i playerA playerB board1 )= do 
--     let (x,y) = validateInputFromUser 
--     let board2 = (Player.addPoint playerA (x,y) board1)
--     if (isOver board2) then (playerWon playerA (i+1) board2) else do
--         let (x,y) = validateInputFromUser 
--         let board3 = (Player.addPoint playerB (x,y) board2)
--         if (isOver board3) then (playerWon playerA (i+1) board3) else (play (Game (i+1) playerA playerB board3))

-- validateInputFromUser:: (Int,Int)
-- validateInputFromUser = do
--     putStrLn "Write x y to make a move"
--     line <- getLine
--     a:_:b <- line
--     if isInteger a && isInteger b then (validatePoint (a,b)) else do
--         show "WRONG FORMAT!"
--         turn player board

-- validatePoint::(Int,Int) -> Color -> Board -> (Int,Int)
-- validatePoint (x,y) board = do
--     if (checkIfPointIsInBoard (Point _ (x,y)) board) && (checkIfPointIsEmpty (Point _ (x,y)) board) then (x,y) else do
--         putStrLn "Wrong coordinates!"
--         dateInputFromUser

playerWon::Player -> Int -> Board -> IO()
playerWon (Player color) turns board= do
    putStrLn (show board)
    putStrLn "\n"
    putStrLn ("PLAYER " ++ (show color) ++ " won in " ++ (show turns) ++ " turns!")

--TODO buggy
checkRow::[Point] -> Color ->  Int -> Int
checkRow (head:ts) colorBefore counter = 
    if colorBefore == Empty && color /= Empty then (checkRow ts color 1) else
        if colorBefore == color && counter < 5 then (checkRow ts color (counter + 1)) else
            if colorBefore == color && counter == 5 && color == Cross then 1 else -- 1 if crosses won
                if colorBefore == color && counter == 5 && color == Circle then 2 else 0-- 2 if circles won
                    where (Point color _) = head 

checkRow [] colorBefore counter = 
    if counter == 5 && colorBefore == Cross then 1 else
        if counter == 5 && colorBefore == Circle then 1 else 0


diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

isOver::Board -> Bool
isOver (Board a b points) = 
    if (sum [(checkRow  (points !! x) Empty 1) | x <- [0..b-1] ]/= 0) then True else
        if (sum [(checkRow (( (transpose . reverse) points) !! x) Empty 1) | x <- [0..a-1] ]/= 0) then True else
            if (sum [(checkRow ((diagonals points) !! x) Empty 1) | x <- [0..b-1] ] /= 0) then True else
                if (sum [(checkRow ((diagonals ( (transpose . reverse) points)) !! x) Empty 1) | x <- [0..a-1] ] /= 0) then True else False
