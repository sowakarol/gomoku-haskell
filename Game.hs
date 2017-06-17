module Game where

import Data.List
import Data.Char
import Data.Tree

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



--getting points in neighbourhood of another points
getPossiblePointsForPlayer:: Board -> [Point]
getPossiblePointsForPlayer (Board a b points) =[(Point color (x+ tmp1,y + tmp2)) | (Point color (x,y)) <- (concat points),not (checkIfPointIsEmpty (Point color (x,y)) (Board a b points)),tmp1 <- [-1, 0, 1], tmp2 <- [-1, 0, 1], x + tmp1 < a, x + tmp1 > 0, y+tmp2 < b, y +tmp2 > 0,checkIfPointIsInBoard (Point color (x + tmp1, y + tmp2)) (Board a b points), checkIfPointIsEmpty (Point color (x + tmp1, y + tmp2)) (Board a b points)  ]

removeDuplicates::[Point] -> [Point]
removeDuplicates x = union x x

--making Boards with a 
makesPossibleBoards :: [Point] -> Board -> Color -> [Board]
makesPossibleBoards ((Point color coordinates):tail) board selectedColor= (addPointToBoard (Point selectedColor coordinates) board) : (makesPossibleBoards tail board color)
makesPossibleBoards [] board color = []

swapColor:: Color -> Color
swapColor Cross = Circle
swapColor Circle = Cross

generateTree:: Player -> Board -> Tree Board 
generateTree (Player color) board = Node board (map (generateTree (Player (swapColor color))) (makesPossibleBoards (removeDuplicates( getPossiblePointsForPlayer board)) board (swapColor color)))

ratePoint:: Point -> Board -> Int
ratePoint (Point color (x,y)) (Board a b points) = 
    (rateVertical (Point color (x,y)) (points !! (x-1)) 0) + (rateVertical (Point color (x,y)) ( (transpose . reverse) points !! (x-1)) 0) + (rateVertical (Point color (x,y)) ((diagonals points) !! (y-1)) 0) + (rateVertical (Point color (x,y)) ((diagonals ( (transpose . reverse) points)) !! (y-1)) 0)

rateVertical::Point -> [Point] -> Int ->Int
rateVertical (Point color tmp) ((Point c (a,b)):ts) rate = if c == color then rateVertical (Point color tmp) ts rate+1 else (rateVertical (Point color tmp) ts 0)
rateVertical (Point color tmp) [] rate = rate
-- getPointIfIsInNeighbourhood:: Point -> Board -> Bool
-- getPointIfIsInNeighbourhood (Point color (x,y)) (Board a b points) = if color == Empty then
--     if (checkIfPointIsInBoard(getPoint (board (x+1,y)) board)) && not(checkIfPointIsEmpty(Point _ (x+1,y)) board)then True else
--         if (checkIfPointIsInBoard(Point _ (x-1,y)) board) && not(checkIfPointIsEmpty(Point _ (x-1,y)) board) then True else
--             if (checkIfPointIsInBoard(Point _ (x,y + 1)) board) && not(checkIfPointIsEmpty(Point _ (x,y + 1)) board) then True else
--                 if (checkIfPointIsInBoard(Point _ (x,y -1)) board) && not(checkIfPointIsEmpty(Point _ (x,y -1)) board) then True else
--                     if (checkIfPointIsInBoard(Point _ (x + 1,y + 1)) board) && not(checkIfPointIsEmpty(Point _ (x + 1,y + 1)) board) then True else
--                         if (checkIfPointIsInBoard(Point _ (x - 1,y + 1)) board) && not(checkIfPointIsEmpty(Point _ (x - 1,y + 1)) board) then True else
--                             if (checkIfPointIsInBoard(Point _ (x-1,y-1)) board) && not(checkIfPointIsEmpty(Point _ (x-1,y-1)) board) then True else
--                                 if (checkIfPointIsInBoard(Point _ (x+1,y-1)) board) && not(checkIfPointIsEmpty(Point _ (x+1,y-1)) board) then True else False
--                                     else False





playerMove :: Game -> IO()
playerMove (Game turns (Player color) otherPlayer board) = do
    putStr ("Write column - Player " ++ (show color) ++ " ")
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
