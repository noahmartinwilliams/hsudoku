module Main where

import System.IO
import Board
import Game

main :: IO ()
main = do
    c <- getContents
    let inputs = getInput c
    let initial = newBoard
    putStrLn (display initial)
