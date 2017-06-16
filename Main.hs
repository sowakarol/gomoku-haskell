import Point
import Color
import Board




main :: IO()
main = do
    putStrLn "Write 'm' if you want to play with a friend or 'c' if you want to play with computer"
    character <- getChar
    --board <- generateEmptyBoard 19 19
    putChar character

