module Board(Board(..), newBoard) where

import Data.Matrix

type Board = Matrix Int

newBoard :: Board
newBoard = zero 9 9 
