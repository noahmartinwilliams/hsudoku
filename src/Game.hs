module Game(display, playGame) where

import Board
import Data.Matrix
import Error
import Control.Monad.State
import Input

display :: Board -> String
display x = do
    (horizLine (5 * 9 + 1) ) ++ "\n" ++ (intern (Data.Matrix.toList x) ) where
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


playGame :: Error (Int, Int, Int) -> Board -> (String, Board)
playGame input b = do
    if (isError input) 
    then
        let (Error (Left str)) = input in ("Error: " ++ str ++ "\n\n" , b)
    else
        let (Error (Right v)) = input in 
            let b' = setValue b v in 
                if (checkForWin b') then ("You Win!\n\n", b') else (display b', b')

