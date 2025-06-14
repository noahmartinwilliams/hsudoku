module Input(Inp(..), getInput, setValue) where

import Data.List.Split
import Text.Read
import Data.Maybe
import Board
import Data.Matrix
import Control.Monad.State

type Inp = (Int, Int, Int)

getInput :: MonadFail m => String -> [m (Int, Int, Int)]
getInput str = do
    let lines = endBy "\n" str
        entryStrs = map (\x -> endBy "," x) lines
        ret = map (\x -> process x) entryStrs
    ret where 
        process :: MonadFail m => [String] -> m (Int, Int, Int)
        process [] = fail "Empty entry"
        process ( a : b : c : _) = do
            a' <- isNum a
            b' <- isNum b
            c' <- isNum c
            return (a', b', c')

        process l = fail ("Invalid input string. Got: " ++ (show l))
            
        isNum :: MonadFail m => String -> m Int
        isNum str = do
            let maybeInt = readMaybe str
            if (isNothing maybeInt) 
            then
                fail ("Expected Integer. Got: " ++ str)
            else
                let (Just i) = maybeInt in return i

setValue :: (Int, Int, Int) -> State Board String
setValue (row, col, val) = state (\x ->  ("", setElem val (row, col) x))
