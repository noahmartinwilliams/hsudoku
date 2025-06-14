module Main where

import System.IO
import Board
import Game
import Input
import Error
import Control.Monad.State

runGame :: [Error (Int, Int, Int)] -> State Board String
runGame [] = state (\b -> ("Game Over\n\n", b))
runGame (head : tail) = do
    str1 <- (playGame head)
    str2 <- runGame tail
    b <- get
    state (\_ -> (str1 ++ str2, b))
    

main :: IO ()
main = do
    c <- getContents
    let inputs = getInput c :: [Error (Int, Int, Int)]
    let initial = newBoard
    let fun = runState (runGame inputs) initial
    let (str, _) = fun 
    putStrLn str

