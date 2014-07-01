{- |
Module      :  TextArea.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

-}

module TextAreaContentUtils where

import Graphics.UI.Gtk
import Data.IORef
import Data.Maybe
import Data.Map as Map
import Control.Monad

import TextAreaContent as TAC

data ContentEntry = ContentEntry Position Char

bef = 15.0
hef = 16.0

captureReturn :: TextAreaContent -> Position -> IO ContentList
captureReturn areaContent (x,y) = do
  content <- enter areaContent (x,y)
  return content

addCharacter :: TextAreaContent -> ContentEntry -> IO () -- Map (remove)
addCharacter areaContent entry@(ContentEntry pos char) = do
  key <- getCell areaContent pos
  when (isJust key) $ do	-- overwrite mode (no shifting of subsequent characters)
    let (k,_) = fromJust key
    when (k /= char) $ do
      res <- deleteCell areaContent pos
      return ()
  putValue areaContent pos char

enter :: TextAreaContent -> Position -> IO ContentList
enter areaContent (x1,y1) = do
  newList <- generateContentList areaContent (\(x2,y2) -> x2 >= x1 && y2 == y1 || y2 > y1)
  shiftDownCharacters areaContent newList
  return newList

shiftDownCharacters :: TextAreaContent -> ContentList -> IO ()
shiftDownCharacters _ [] = return ()
shiftDownCharacters areaContent (((x,y), char):tail) = do
  shiftDownCharacters areaContent tail
  putValue areaContent (x,y+hef) char
  res <- deleteCell areaContent (x,y)
  return ()

backSpaceLine :: Double -> TextAreaContent -> Adjustment -> IO (ContentList,Position,ContentList) 
backSpaceLine y1 areaContent hAdjustment = do
  previousLine <- generateContentList areaContent (\(_,y) -> y == y1-hef)
  let shift = if Prelude.null previousLine then 0 else bef
  let (x2,y2) = if Prelude.null previousLine then (0,y1-hef) else fst $ maximum previousLine
  firstLine <- shiftUpFirstLine areaContent (x2+shift,y2) y1 hAdjustment
  subsequentLines <- shiftUpSubsequentLines areaContent (x2+shift,y2) y1 hAdjustment  
  return (firstLine,(x2+shift,y2),reverse subsequentLines)

shiftUpFirstLine :: TextAreaContent -> Position -> Double -> Adjustment -> IO ContentList
shiftUpFirstLine areaContent pos y1 hAdjustment = do
  currentLine <- generateContentList areaContent (\(_,y) -> y == y1) -- depends on entry mode  
  shiftUpCharacters areaContent pos currentLine
  return currentLine

shiftUpSubsequentLines :: TextAreaContent -> Position -> Double -> Adjustment -> IO ContentList -- dep: shiftCharacters + Map
shiftUpSubsequentLines areaContent pos y1 hAdjustment = do
  nextLinesMap <- generateContentList areaContent (\(_,y) -> y > y1)
  shiftCharacters areaContent nextLinesMap 0 (-hef)
  return nextLinesMap

shiftUpCharacters :: TextAreaContent -> Position -> ContentList -> IO () -- dep: addToContent + Map
shiftUpCharacters _ _ [] = return ()
shiftUpCharacters areaContent (x1,y1) (((x2,y2),char):xs) = do
  deleteCell areaContent (x2,y2)
  putValue areaContent (x2+x1,y1) char
  shiftUpCharacters areaContent (x1,y1) xs

shiftLeftLine :: TextAreaContent -> Position -> Adjustment -> IO ContentList -- dep: shiftCharacters + Map
shiftLeftLine areaContent (x,y) hAdjustment = do
  deleteCell areaContent (x,y)
  currentLineList <- generateContentList areaContent (\(x2,y2) -> x2 > x && y2 == y) -- depends on entry mode 
  shiftCharacters areaContent currentLineList (-bef) 0
  return $ reverse currentLineList

shiftCharacters :: TextAreaContent -> ContentList -> Double -> Double -> IO () -- dep: addToContent + Map
shiftCharacters _ [] _ _ = return ()
shiftCharacters areaContent ((pos@(x,y),char):xs) bef hef = do
  deleteCell areaContent pos
  putValue areaContent (x+bef,y+hef) char
  shiftCharacters areaContent xs bef hef

updatePosition :: Position -> IO Position
updatePosition (x,y) = 
  return (leftX,topY)
  where leftX = getMultiplier x bef 1
        topY  = getMultiplier y hef 1
  
getMultiplier :: Double -> Double -> Double -> Double
getMultiplier a b count
  | a > newValue = getMultiplier a b (count+1)
  | otherwise = b * (count-1)
  where newValue = b * count
