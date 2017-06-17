module Main where

import Point
import Color
import Board
import Game
import Player


main :: IO()
main = do
    putStrLn "Write 'm' if you want to play with a friend or 'c' if you want to play with computer"
    character <- getChar
    --board <- generateEmptyBoard 19 19
    if character == 'm' then loopTwoPlayers else if character == 'c' then loop else initializationError

initializationError = do
    tmp <- getChar
    putStrLn "Wrong argument!"
    main

loopTwoPlayers :: IO()
loopTwoPlayers = do
    play (Game 0 (Player Cross) (Player Circle) (generateEmptyBoard 19 19)) 

loop:: IO()
loop = do
    putChar 'c'

