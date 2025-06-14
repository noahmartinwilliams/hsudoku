module Board(Board(..), newBoard, checkForWin) where

import Data.Matrix
import Data.List
import Data.List.Split
import Control.Parallel

type Board = Matrix Int

newBoard :: Board
newBoard = zero 9 9  -- TODO: Figure out how to generate a random board that can still be beaten.

checkForWin :: Board -> Bool
checkForWin b = let b' = Data.Matrix.toList b in (checkRows b') && (checkColumns b) && (checkBoxes b') where
    checkRows :: [Int] -> Bool
    checkRows [] = True
    checkRows ls = do
        let ls' = take 9 ls
            rest = drop 9 ls
            sorted = sort ls'
            restChecked = checkRows rest
        par restChecked ((sorted == [1, 2, 3, 4, 5, 6, 7, 8, 9]) && restChecked)

    checkColumns :: Board -> Bool
    checkColumns b = do
        let b2 = Data.Matrix.toList (Data.Matrix.transpose b)
        checkRows b2

    checkBoxes :: [Int] -> Bool
    checkBoxes ls = do
        let chunks = chunksOf 3 ls
            boxes = getBoxes [0,1,2,9,10,11,18,19,20] chunks
        checkAllRows boxes

    checkAllRows :: [[Int]] -> Bool
    checkAllRows [] = True
    checkAllRows (ls : rest) = let restChecked = checkAllRows rest in 
        par restChecked ((checkRows ls) && restChecked)

    getBoxes:: [Int] -> [[Int]] -> [[Int]]
    getBoxes [] _  = []
    getBoxes (head : tail) ls = (buildBox (drop head ls) ) ++ (getBoxes tail ls)

    buildBox :: [[Int]] -> [[Int]]
    buildBox ( a : _ : _ : b2 : _ : _ : c : _) = [a ++ b2 ++ c]
