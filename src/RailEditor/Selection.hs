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
                    getCellsByPositons
                 )
  where

import qualified Data.Map as Map
import Data.IORef
import Control.Monad
import Data.Maybe
import qualified TextAreaContent as TAC
import qualified Data.List as List

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
  let char = fst $ fst $ fromJust cell
  TAC.deleteCell tac pos
  TAC.putCell tac pos ((char,value),TAC.defaultColor)

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
  let char = fst $ fst $ fromJust cell
  --TAC.deleteCell tac oldPos   -- for cut and paste
  TAC.putCell tac (newX+x-offsetX,newY+y-offsetY) ((char,False),TAC.defaultColor)

getCellsByPositons :: TAC.TextAreaContent -> [TAC.Position] -> IO [(Char,Bool)]
getCellsByPositons tac postions = _getCellsByPositons tac postions []

_getCellsByPositons :: TAC.TextAreaContent -> [TAC.Position] -> [(Char,Bool)] -> IO [(Char,Bool)]
_getCellsByPositons _ [] cells = return cells
_getCellsByPositons tac (x:xs) cells = do
  cell <- TAC.getCell tac x 
  let (content,_) = fromJust cell
  _getCellsByPositons tac xs (content:cells)

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

{-
selectAll :: DrawingArea -> ContentRef -> IO ()
selectAll drawingArea contentRef = do
  content <- readIORef contentRef
  let unSelectedContentEntries = Map.filter (\contentEntry -> isSelected contentEntry == False) content
  when (Map.size unSelectedContentEntries > 0) $ do
    selectEntries drawingArea $ Map.toList unSelectedContentEntries
    modifyIORef' contentRef $ Map.map $ \(Content ch co _) -> (Content ch co True)
-}

getMaximumY :: [TAC.Position] -> TAC.Coord
getMaximumY = Prelude.foldl (\y1 (_,y2) -> max y1 y2) 0

getMaximumX :: [TAC.Position] -> TAC.Coord
getMaximumX = Prelude.foldl (\x1 (x2,_) -> max x1 x2) 0

getMinimumY :: [TAC.Position] -> TAC.Coord
getMinimumY positions = fst $ minimum $ Prelude.map (\(x,y) -> (y,x)) positions

getMinimumX :: [TAC.Position] -> TAC.Coord
getMinimumX = fst.minimum

getTopLeft :: [TAC.Position] -> TAC.Position
getTopLeft positions = (getMinimumX positions, getMinimumY positions)

getBottomRight :: [TAC.Position] -> TAC.Position
getBottomRight positions = (x+1,y)
  where (y,x) = maximum $ Prelude.map (\(x1,y1) -> (y1,x1)) positions
