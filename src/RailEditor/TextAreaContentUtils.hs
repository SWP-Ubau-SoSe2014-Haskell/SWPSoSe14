{- |
Module      :  TextAreaContentUtils.hs
Description :  .
Maintainer  :  Kristin Knorr (c)
License     :  MIT

Stability   :  stable

'TextAreaContentUtils' serves methods to move Characters in TextAreaContent.

-}

module TextAreaContentUtils (
-- * Methods
  moveChars,
  findLastCharBefore,
  moveLinesUp,
  moveLinesDownXShift,
  moveCharsRight,
  mvLinesUp
  ) where

import Graphics.UI.Gtk
import Data.IORef
import Data.Maybe
import Data.Map as Map
import Control.Monad

import qualified TextAreaContent as TAC


-- | calculates Destination depending on Direction
calculateDest :: TAC.Position
  -> TAC.Direction
  -> TAC.Position
calculateDest (stX,stY) (dirX,dirY) = (stX+dirX,stY+dirY)

-- | moves Contents into a Direction
moveChar :: TAC.TextAreaContent
  -> TAC.Position
  -> TAC.Direction
  -> IO()
moveChar area from dir = do
  x <- TAC.getCell area from
  unless (isNothing x) $ do
    let
      cell = fromJust x
      to = calculateDest from dir
    TAC.putCell area to cell
    TAC.deleteCell area from
    return ()

-- | moves amount of Characters of one line in range from Pos x to last char in line
moveChars :: TAC.TextAreaContent
  -> TAC.Position
  -> TAC.Direction
  -> IO()
moveChars area (stX, line) dir = do
  endX <- TAC.findLastChar area line
  unless (stX > endX) $
    if snd dir == 0 && fst dir > 0
    then
      moveCharsRight area stX endX line dir
    else do
      moveChar area (stX,line) dir
      moveChars area (stX+1,line) dir
      return ()
  where
    moveCharsRight area stX endX line dir = 
      unless (stX > endX) $ do
        moveChar area (endX,line) dir
        moveCharsRight area stX (endX-1) line dir
        return ()

{- |
 searchs for last character in Line and returns x-Position, if Line is empty
 return -1
-}

findLastCharBefore :: TAC.TextAreaContent
  -> TAC.Coord
  -> TAC.Coord
  -> IO TAC.Coord
findLastCharBefore area x line =
  if x<0
  then return (-1)
  else do
    cont <- TAC.getCell area (x,line)
    if isJust cont
    then return x
    else findLastCharBefore area (x-1) line

-- | searchs for last written Line
findLastWrittenLine :: TAC.TextAreaContent
  -> IO TAC.Coord
findLastWrittenLine area = do
  size <- TAC.size area
  findLastWrittenLineHelper area (snd size)
  where
    findLastWrittenLineHelper area line =
      if line<0
      then return(-1)
      else do
        empty <- TAC.isEmptyLine area line
        if empty
        then findLastWrittenLineHelper area (line-1)
        else return line

-- | moves Lines up where param line is the upper line
moveLinesUp :: TAC.TextAreaContent
  -> TAC.Coord
  -> IO()
moveLinesUp area line = do
  finY <- findLastWrittenLine area
  moveLinesUpHelper area line line finY
  where
      moveLinesUpHelper area line stY finY = 
        unless (line<=0 || line>finY) $ do
          empty <- TAC.isEmptyLine area line
          if empty
          then moveLinesUpHelper area (line+1) stY finY
          else
            if line == stY
            then do
              lastPrev <- TAC.findLastChar area (line-1)
              moveChars area (0,line) (lastPrev+1, -1)
              moveLinesUpHelper area (line+1) stY finY
            else do
              moveChars area (0,line) (0,-1)
              moveLinesUpHelper area (line+1) stY finY

{- |
 moves Lines down where param line upper Line
 param xShift is a Boolean, which defines wether
 the upper line starting at posx is shifted vertically down (False)
 or is shifted down to pos 0 (True)
-}
moveLinesDownXShift :: TAC.TextAreaContent
  -> TAC.Position
  -> Bool
  -> IO()
moveLinesDownXShift area (posX,line) xShift = do
  lastLine <- findLastWrittenLine area
  unless (line>lastLine || line<0) $
    if line==lastLine
    then 
      moveChars area (posX,line) $
        if xShift then (-posX,1) else (0,1)
    else
      if xShift
      then do
        moveLinesVertDown area (line+1)
        moveChars area (posX,line) (-posX,1)
      else do
        moveLinesVertDown area (line+1)
        moveChars area (posX,line) (0,1)

-- | moves all chars of lines lower "line" to one line lower
moveLinesVertDown :: TAC.TextAreaContent
  -> TAC.Coord
  -> IO()
moveLinesVertDown area line = do
  lastLine <- findLastWrittenLine area
  moveDownHelper area lastLine line
  where
    moveDownHelper area line stY =
      unless (line<stY) $ do
        empty <- TAC.isEmptyLine area line
        if empty
        then moveDownHelper area (line-1) stY
        else do
          moveChars area (0,line) (0,1)
          moveDownHelper area (line-1) stY

moveCharsRight :: TAC.TextAreaContent -> TAC.Position -> TAC.Position -> TAC.Position -> IO TAC.Position
moveCharsRight tac (x,y) topLeft@(xLeft,yTop) bottomRight@(xRight,yBottom) = do
  moveChars tac bottomRight $ if (x,y) == topLeft then (x - xRight - 1, y - yBottom) else (xLeft - x, yTop - y)
  return topLeft

mvLinesUp :: TAC.TextAreaContent -> TAC.Coord -> Int -> (TAC.Action, TAC.Position) -> IO (TAC.Action, TAC.Position)
mvLinesUp _ _ 0 action = return action
mvLinesUp tac y diff action = do 
  moveLinesUp tac y
  mvLinesUp tac (y-1) (diff-1) (TAC.Concat action (TAC.RemoveLine, (0,y-1)), (0, y))

