module EditorContent where

import Graphics.UI.Gtk
import Data.IORef
import Data.Maybe
import Data.Map as Map
import Control.Monad

import EditorTypes

captureReturn :: Content -> Position -> IO (ContentList)
captureReturn contentRef (x,y) = do
  content <- enter contentRef (x,y)
  return content

addCharacter :: Content -> ContentEntry -> IO ()
addCharacter contentRef entry@(ContentEntry pos char) = do
  content <- readIORef contentRef
  let key = Map.lookup pos content
  if isJust key then do	-- overwrite mode (no shifting of subsequent characters)
    when (fromJust key /= char) $ do
      modifyIORef' contentRef (Map.delete pos)
      addToContent contentRef entry
  else addToContent contentRef entry

addToContent :: Content -> ContentEntry -> IO ()
addToContent contentRef (ContentEntry pos char)  = do
  modifyIORef' contentRef (Map.insert pos char)

enter :: Content -> Position -> IO (ContentList)
enter contentRef (x1,y1) = do
  content <- readIORef contentRef
  let newList = Map.toList $ Map.filterWithKey (\(x2,y2) _ -> x2 >= x1 && y2 == y1 || y2 > y1) content
  shiftDownCharacters contentRef newList
  return newList

shiftDownCharacters :: Content -> ContentList -> IO ()
shiftDownCharacters _ [] = return ()
shiftDownCharacters contentRef (((x,y), char):tail) = do
  shiftDownCharacters contentRef tail
  addToContent contentRef (ContentEntry (x,y+hef) char)
  modifyIORef' contentRef (Map.delete (x,y))

backSpaceLine :: Double -> Content -> Adjustment -> IO (ContentList,Position,ContentList)
backSpaceLine y1 contentRef hAdjustment = do
  content <- readIORef contentRef
  let previousLine = Map.toList $ Map.filterWithKey (\(_,y) _ -> y == y1-hef) content
      shift = if Prelude.null previousLine then 0 else bef
      (x2,y2) = if Prelude.null previousLine then (0,y1-hef) else fst $ maximum previousLine
  firstLine <- shiftUpFirstLine contentRef (x2+shift,y2) y1 hAdjustment
  subsequentLines <- shiftUpSubsequentLines contentRef (x2+shift,y2) y1 hAdjustment  
  return (firstLine,(x2+shift,y2),reverse subsequentLines)

shiftUpFirstLine :: Content -> Position -> Double -> Adjustment -> IO (ContentList)
shiftUpFirstLine contentRef pos y1 hAdjustment = do
  content <- readIORef contentRef  
  let currentLine = Map.toList $ Map.filterWithKey (\(_,y) _ -> y == y1) content	-- depends on entry mode  
  shiftUpCharacters contentRef pos currentLine
  return currentLine

shiftUpSubsequentLines :: Content -> Position -> Double -> Adjustment -> IO (ContentList)
shiftUpSubsequentLines contentRef pos y1 hAdjustment = do
  content <- readIORef contentRef
  let nextLinesMap = Map.toList $ Map.filterWithKey (\(_,y) _ -> y > y1) content      
  shiftCharacters contentRef nextLinesMap 0 (-hef)
  return nextLinesMap

shiftUpCharacters :: Content -> Position -> ContentList -> IO ()  
shiftUpCharacters _ _ [] = return ()  
shiftUpCharacters contentRef (x1,y1) (((x2,y2),char):xs) = do
  modifyIORef' contentRef (Map.delete (x2,y2))
  addToContent contentRef (ContentEntry (x2+x1,y1) char)
  shiftUpCharacters contentRef (x1,y1) xs  

shiftLeftLine :: Content -> Position -> Adjustment -> IO (ContentList)
shiftLeftLine contentRef (x,y) hAdjustment = do
  content <- readIORef contentRef
  modifyIORef' contentRef (Map.delete (x,y))
  let currentLineMap = Map.filterWithKey (\(x2,y2) _ -> x2 > x && y2 == y) content	-- depends on entry mode 
  shiftCharacters contentRef (Map.toList currentLineMap) (-bef) 0 
  return $ reverse $ Map.toList currentLineMap

shiftCharacters :: Content -> ContentList -> Double -> Double -> IO ()  
shiftCharacters _ [] _ _ = return ()  
shiftCharacters contentRef ((pos@(x,y),char):xs) bef hef = do
  modifyIORef' contentRef (Map.delete pos)
  addToContent contentRef (ContentEntry (x+bef,y+hef) char)
  shiftCharacters contentRef xs bef hef   
   
updatePosition :: Position -> IO (Position)
updatePosition (x,y) = 
  return (leftX,topY)
  where leftX = getMultiplier x bef 1
        topY  = getMultiplier y hef 1
  
getMultiplier :: Double -> Double -> Double -> Double
getMultiplier a b count
  | a > newValue = getMultiplier a b (count+1)
  | otherwise = b * (count-1)
  where newValue = b * count
