module Game where

import Data.List
import Data.Char

import Board
import Player
import Point
import Color


data Game = Game{numberOfTurns::Int, playerA::Player, playerB::Player, board::Board}

-- play::Game -> IO()
-- play (Game i playerA playerB board1 )= do 
--     putStrLn "Write x y to make a move"
--     line <- getLine
--     let a:_:b = line
--     if isInteger a && isInteger b then (validatePoint (a,b)) else do
--         putStrLn "WRONG FORMAT!"



-- validateInputFromUser = do
-- let board2 = (Player.addPoint playerA (x,y) board1)
-- if (isOver board2) then (playerWon playerA (i+1) board2) else do
--     let (x,y) = validateInputFromUser 
--     let board3 = (Player.addPoint playerB (x,y) board2)
--     if (isOver board3) then (playerWon playerA (i+1) board3) else (play (Game (i+1) playerA playerB board3))

-- validatePoint::(Int,Int) -> Color -> Board -> (Int,Int)
-- validatePoint (x,y) board =
--     if (checkIfPointIsInBoard (Point _ (x,y)) board) && (checkIfPointIsEmpty (Point _ (x,y)) board) then (x,y) else do
--         show "Wrong coordinates!"
--         validateInputFromUser

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False



playerMove :: Game -> IO()
playerMove (Game turns (Player color) otherPlayer board) = do
    putStr ("Write column - Player " ++ (show color))
    x <- getLine
    if (isInteger x) then do 
        putStr "Write row: "
        y <- getLine
        if (isInteger y) then do 
            let boardAfterMove = (addPointToBoard (Point color (read x::Int, read y)) board)
            if (boardAfterMove == board) then do 
                putStrLn "Wrong x y, try again!"
                playerMove (Game turns (Player color) otherPlayer board)
                else do
                    putStrLn $ show boardAfterMove
                    turn (Game (turns+1) otherPlayer (Player color) boardAfterMove)
            else do
                putStrLn "Wrong value, try again!"
                playerMove (Game turns (Player color) otherPlayer board)
        else do
            putStrLn "Wrong value, try again!"
            playerMove (Game turns (Player color) otherPlayer board)




--         putStr "Write row: "
--         y <- getLine
--         if (isInteger y) then do
--         let boardAfterMove = (addPointToBoard (Point color (read x::Int, read y)) board)
--             if (boardAfterMove == board) then do
--                 putStrLn "Wrong x y, try again!"
--                 playerMove board (Player color)
--             else do 
--                     putStrLn $ show boardAfterMove
--                     turn (turns+1) otherPlayer (Player color) boardAfterMove
--         else
--             putStrLn "Wrong value, try again!"
--             playerMove board (Player color)
--     else do
--         putStrLn "Wrong value, try again!"
--         playerMove board (Player color)

-- if x then do
--     putStr ""
--     else do




turn :: Game -> IO()
turn (Game turns currentPlayer otherPlayer board) = do
    if(isOver board) then
        playerWon otherPlayer turns board
    else do
        playerMove (Game turns currentPlayer otherPlayer board)


playerWon::Player -> Int -> Board -> IO()
playerWon (Player color) turns board= do
    putStrLn "\n"
    putStrLn (show board)
    putStrLn "\n"
    let number = (turns `div` 2)
    putStrLn ("PLAYER " ++ (show color) ++ " won in " ++ (show number) ++ " turns!")

checkRow::[Point] -> Color ->  Int -> Int
checkRow (head:ts) colorBefore counter = 
    if colorBefore == Empty then (checkRow ts color 1) else
        if colorBefore == color && (counter + 1) < 5 then (checkRow ts color (counter + 1)) else
            if colorBefore == color && (counter + 1) == 5 && color == Cross then 1 else -- 1 if crosses won
                if colorBefore == color && (counter + 1) == 5 && color == Circle then 2 else 0-- 2 if circles won
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
