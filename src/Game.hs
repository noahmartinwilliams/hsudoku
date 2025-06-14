module Game(display, playGame) where

import Board
import Data.Matrix
import Error
import Control.Monad.State
import Input

display :: State Board String
display = do
    gs <- get
    if (checkForWin gs) 
    then
        return "You win!!\n\n"
    else
        return ((horizLine (5 * 9 + 1)) ++ "\n" ++ (intern (Data.Matrix.toList gs))) where

    intern :: [Int] -> String
    intern [] = ""
    intern input = do
        let row = take 9 input
            rest = drop 9 input
        (intern2 row) ++ "|\n" ++ (horizLine (5 * 9 + 1)) ++ "\n" ++ (intern rest)

    intern2 :: [Int] -> String
    intern2 [] = ""
    intern2 (a : rest) = "|" ++ "  " ++ (show a) ++ " " ++ (intern2 rest)
        
    horizLine :: Int -> String
    horizLine 0 = ""
    horizLine x = "-" ++ (horizLine (x-1))


playGame :: Error (Int, Int, Int) -> State Board String
playGame input = do
    if (isError input) 
    then
        let (Error (Left str)) = input in return ("Error: " ++ str ++ "\n\n" ) 
    else
        let (Error (Right v)) = input in 
            (setValue v) >> display

