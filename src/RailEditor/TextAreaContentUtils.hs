{- |
Module      :  TextArea.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

-}

module TextAreaContentUtils (
-- * Detail
--
-- | 'TextAreaContentUtils' serves methods to move Characters in TextAreaContent.

-- * Types
  Direction,

-- * Methods
  moveChars,
  findLastChar,
  findLastCharBefore,
  moveLinesUp,
  moveLinesDownXShift
  ) where

import Graphics.UI.Gtk
import Data.IORef
import Data.Maybe
import Data.Map as Map
import Control.Monad

import qualified TextAreaContent as TAC

type Direction = (TAC.Coord,TAC.Coord)

-- / calculates Destination depending on Direction
calculateDest :: TAC.Position
  -> Direction
  -> TAC.Position
calculateDest (stX,stY) (dirX,dirY) = (stX+dirX,stY+dirY)

-- / moves Contents into a Direction
moveChar :: TAC.TextAreaContent
  -> TAC.Position
  -> Direction
  -> IO()
moveChar area from dir = do
  x <- TAC.getCell area from
  unless (isNothing x) $ do
    let
      (char,col) = fromJust x
      to = calculateDest from dir
    TAC.putCell area to (char,col)
    TAC.deleteCell area from
    return ()

-- / moves amount of Characters of one line
moveChars :: TAC.TextAreaContent
  -> TAC.Coord
  -> TAC.Coord
  -> TAC.Coord
  -> Direction
  -> IO()
moveChars area stX endX line dir =
  unless (stX > endX) $
    if snd dir == 0 && fst dir > 0
    then do
      moveChar area (endX,line) dir
      moveChars area stX (endX-1) line dir
      return ()
    else do
      moveChar area (stX,line) dir
      moveChars area (stX+1) endX line dir
      return ()

-- / searchs for last character in Line and returns x-Position, if Line is empty
--   return -1
findLastChar :: TAC.TextAreaContent
  -> TAC.Coord
  -> IO TAC.Coord
findLastChar area line = do
  size <- TAC.size area
  findLastCharBefore area (fst size) line

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

-- / searchs for last written Line
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
        finalSelf <- findLastChar area line
        if finalSelf == (-1)
        then findLastWrittenLineHelper area (line-1)
        else return line

-- / moves Lines up
moveLinesUp :: TAC.TextAreaContent
  -> TAC.Coord
  -> IO()
moveLinesUp area line = do
  finY <- findLastWrittenLine area
  moveLinesUpHelper area line line finY
  where
      moveLinesUpHelper area line stY finY = 
        unless (line<=0 || line>finY) $ do
          lastSelf <- findLastChar area line
          if lastSelf==(-1)
          then moveLinesUpHelper area (line+1) stY finY
          else
            if line == stY
            then do
              lastPrev <- findLastChar area line
              moveChars area 0 lastSelf line (lastPrev+1, -1)
              moveLinesUpHelper area (line+1) stY finY
            else do
              moveChars area 0 lastSelf line (0,-1)
              moveLinesUpHelper area (line+1) stY finY

moveLinesDownXShift :: TAC.TextAreaContent
  -> TAC.Position
  -> Bool
  -> IO()
moveLinesDownXShift area (posX,line) xShift = do
  lastLine <- findLastWrittenLine area
  lastSelf <- findLastChar area line
  unless (line<lastLine || line<0) $
    if line==lastLine
    then 
      moveChars area posX lastSelf line $
        if xShift then (-posX,1) else (0,1)
    else
      if xShift
      then do
        moveLinesVertDown area (line+1)
        moveChars area posX lastSelf line (-posX,1)
      else do
        moveLinesVertDown area (line+1)
        moveChars area posX lastSelf line (0,1)

moveLinesVertDown :: TAC.TextAreaContent
  -> TAC.Coord
  -> IO()
moveLinesVertDown area line = do
  lastLine <- findLastWrittenLine area
  moveDownHelper area lastLine line
  where
    moveDownHelper area line stY =
      unless (line<stY) $ do
        lastSelf <- findLastChar area line
        if lastSelf==(-1)
        then moveDownHelper area (line-1) stY
        else do
          moveChars area 0 lastSelf line (0,1)
          moveDownHelper area (line-1) stY
