{- |
Module      :  Selection.hs
Description :  .
Maintainer  :  Benjamin Kodera (c)
License     :  MIT

Stability   :  stable

The Selection-module handles multiple character selection as well as copy and paste functionality.
-}

module Selection (
                    handleSelection,
                    updateCells,
                    relocateCells,
                    clear,
                    getFirstPositions,
                    getBottomRight,
                    getMinimum,
                    getCellsByPositons
                 )
  where

import qualified Data.Map as Map
import Data.IORef
import Control.Monad
import Data.Maybe
import qualified TextAreaContent as TAC
import qualified Data.List as List
import qualified RedoUndo as History

handleSelection :: TAC.TextAreaContent -> TAC.Position -> TAC.Position -> IO (Bool,[TAC.Position])
handleSelection tac currentPos newPos = do
  positions <- TAC.getPositons tac
  let selectedEntries = getSelectedEntries positions currentPos newPos
  if not (null selectedEntries) then do
    cell <- TAC.getCell tac $ head selectedEntries
    let isAlreadySelected = snd $ fst $ fromJust cell
    updateCells tac selectedEntries $ not isAlreadySelected 
    return (isAlreadySelected,selectedEntries)
  else return (False,[])
  
updateCells :: TAC.TextAreaContent -> [TAC.Position] -> Bool -> IO ()
updateCells _ [] _ = return ()
updateCells tac (pos:xs) value = do
  updateCell tac pos value
  updateCells tac xs value

updateCell :: TAC.TextAreaContent -> TAC.Position -> Bool -> IO ()  
updateCell tac pos value = do
  cell <- TAC.getCell tac pos
  when (isJust cell) $ do
    let ((char,_),color) = fromJust cell
    TAC.deleteCell tac pos
    TAC.putCell tac pos ((char,value),color)

relocateCells :: TAC.TextAreaContent -> [TAC.Position] -> TAC.Position -> IO TAC.Position
relocateCells tac [] pos = return pos
relocateCells tac positions (x,y) = do
  let (x1,y1) =  getTopLeft positions
      (x2,y2) = getBottomRight positions
  _relocateCells tac positions (x,y) (x1,y1)
  return (x+x2-x1,y+y2-y1)

_relocateCells :: TAC.TextAreaContent -> [TAC.Position] -> TAC.Position -> TAC.Position -> IO ()
_relocateCells _ [] _ _ = return ()
_relocateCells tac (pos:xs) newPos offset = do
  relocateCell tac pos newPos offset
  _relocateCells tac xs newPos offset
  
relocateCell :: TAC.TextAreaContent -> TAC.Position -> TAC.Position -> TAC.Position -> IO ()  
relocateCell tac pos@(x,y) (newX,newY) (offsetX,offsetY) = do
  cell <- TAC.getCell tac pos
  when (isJust cell) $ do
    let char = fst $ fst $ fromJust cell
    TAC.putCell tac (newX+x-offsetX,newY+y-offsetY) ((char,False),TAC.defaultColor)

-- | clears a selection and removes all selected characters
clear :: TAC.TextAreaContent -> TAC.Position -> IO (TAC.Position,TAC.Position)
clear tac pos@(x,y) = do
  positions <- TAC.getSelectedPositons tac
  if not (null positions) then do
    cells <- Selection.getCellsByPositons tac positions
    History.action tac (getMinimum positions) (TAC.Remove cells)
    clearCells tac positions
    return (getMinimum positions,getMaximum positions)
  else return ((x-1,y),(x-1,y))

clearCells :: TAC.TextAreaContent -> [TAC.Position] -> IO ()
clearCells tac [] = return ()
clearCells tac (x:xs) = do
  TAC.deleteCell tac x
  clearCells tac xs

getCellsByPositons :: TAC.TextAreaContent -> [TAC.Position] -> IO [(Char,Bool)]
getCellsByPositons _ [] = return []
getCellsByPositons tac positions = _getCellsByPositons tac positions []

_getCellsByPositons :: TAC.TextAreaContent -> [TAC.Position] -> [(Char,Bool)] -> IO [(Char,Bool)]
_getCellsByPositons _ [] cells = return cells
_getCellsByPositons tac (x:xs) cells = do
  cell <- TAC.getCell tac x 
  if isJust cell then do 
    let (content,_) = fromJust cell
    _getCellsByPositons tac xs (content:cells)
  else _getCellsByPositons tac xs cells

getSelectedEntries :: [TAC.Position] -> TAC.Position -> TAC.Position -> [TAC.Position]
getSelectedEntries positions (x1,y1) (x2,y2)
  -- down
  | y2 > y1 = List.filter (\(x,y) -> x >= x1 && y == y1 || y > y1 && y < y2 || x < x2 && y == y2) positions
  -- right
  | x2 > x1 && y2 == y1 = List.filter (\(x,y) -> x >= x1 && x < x2 && y == y2) positions 
  -- left
  | x2 < x1 && y2 == y1 = List.filter (\(x,y) -> x < x1 && x >= x2 && y == y2) positions 
  -- up
  | otherwise = List.filter (\(x,y) -> x < x1 && y == y1 || y < y1 && y > y2 || x >= x2 && y == y2) positions

getMaximumY :: [TAC.Position] -> TAC.Coord
getMaximumY = Prelude.foldl (\y1 (_,y2) -> max y1 y2) 0

getMaximumX :: [TAC.Position] -> TAC.Coord
getMaximumX = Prelude.foldl (\x1 (x2,_) -> max x1 x2) 0

getMinimumY :: [TAC.Position] -> TAC.Coord
getMinimumY [] = 0
getMinimumY positions = fst $ minimum $ Prelude.map (\(x,y) -> (y,x)) positions

getMinimumX :: [TAC.Position] -> TAC.Coord
getMinimumX [] = 0
getMinimumX positions = fst $ minimum positions

getTopLeft :: [TAC.Position] -> TAC.Position
getTopLeft [] = (0,0)
getTopLeft positions = (getMinimumX positions, getMinimumY positions)

getBottomRight :: [TAC.Position] -> TAC.Position
getBottomRight [] = (0,0)
getBottomRight positions = (x+1,y)
  where (y,x) = maximum $ Prelude.map (\(x1,y1) -> (y1,x1)) positions
  
getMinimum :: [TAC.Position] -> TAC.Position
getMinimum [] = (0,0)
getMinimum positions = (x,y) 
  where (y,x) = minimum $ Prelude.map (\(x1,y1) -> (y1,x1)) positions

getMaximum :: [TAC.Position] -> TAC.Position
getMaximum [] = (0,0)
getMaximum positions = (x,y) 
  where (y,x) = maximum $ Prelude.map (\(x1,y1) -> (y1,x1)) positions

getFirstPositions :: [TAC.Position] -> [TAC.Position]
getFirstPositions [] = []
getFirstPositions positions = List.map minimum $ List.groupBy (\(x1,y1) (x2,y2) -> y1 == y2) positions
