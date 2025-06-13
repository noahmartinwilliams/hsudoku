module Main where

import System.IO
import Board
import Game
import Input
import Error

runGame :: [Error (Int, Int, Int)] -> Board -> (String, Board)
runGame [] b = ("Game Over\n\n", b)
runGame (head : tail) b = let (str, b') = playGame head b in 
    let (str', b'') = runGame tail b' in
        (str ++ str', b'')

main :: IO ()
main = do
    c <- getContents
    let inputs = getInput c :: [Error (Int, Int, Int)]
    let initial = newBoard
    let (str, _) = runGame inputs initial
    putStrLn str

