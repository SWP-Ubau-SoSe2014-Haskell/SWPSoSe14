{- |
Module      :  TextAreaContent.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

This TextAreaContent-module stores two data structures that saves all entries for the editor.
The first data structur saves the text-entries, the second data structure saves the color-entries.
-}
module TextAreaContent (
                        TextAreaContent.init,   -- initializes both data structures
                        serialize,   -- serializes both data structures
                        deserialize,   -- deserializes both data structures
                        putValue,   -- sets the text-entry for a certian point in the data structure
                        putColor,   -- sets the color for a certian point in the data structure
                        getCell,   -- reads the data form both datastructures (text and color)
                        TextAreaContent.size
                       ) where

import Graphics.UI.Gtk
import Data.Map as Map
import Data.IORef
import Data.Maybe

data ColorMap = CoMap  (IORef (Map (Int,Int) Color)) (IORef (Int,Int))
data CharMap  = ChMap  (IORef (Map (Int,Int) Char)) (IORef (Int,Int))

type TextAreaContent = (CharMap,ColorMap)

-- Constants
red = (Color 65535 0 0)
blue = (Color 2478 13810 63262)
green = (Color 3372 62381 5732)
gold = (Color 65535 30430 0)
black = (Color 0 0 0)

defaultColor = black
defaultChar = ' '

init :: Int -> Int -> IO (TextAreaContent)
init x y = do
    size <- newIORef (x,y)
    hmapR <- newIORef hmap
    cmapR <- newIORef cmap
    let colorMap  = CoMap cmapR size
    let charMap = ChMap hmapR size
    return (charMap,colorMap)
        where hmap = fillMapWith (Map.empty) defaultChar x y x y
              cmap = fillMapWith (Map.empty) defaultColor x y x y


fillMapWith :: Map.Map (Int,Int) a -> a -> Int -> Int -> Int -> Int -> Map.Map (Int,Int) a
fillMapWith map content _ _ 0 0 = Map.insert (0,0) content map
fillMapWith map content xBound yBound 0 y = fillMapWith (Map.insert (0,y) content map) content xBound yBound xBound (pred y)
fillMapWith map content xBound yBound x y = fillMapWith (Map.insert (x,y) content map) content xBound yBound (pred x) y

serialize :: TextAreaContent -> IO (String)
serialize ((ChMap hMap size),_) = do
  hmap <- readIORef hMap
  let list = toList hmap
  let sortedList = quicksort list
  result <- listToString sortedList [] 0
  let rightOrder = unlines $ Prelude.map (reverse . dropWhile (== ' ') . reverse) (lines $ reverse result)
  return rightOrder
    where
      quicksort :: [((Int,Int),Char)] -> [((Int,Int),Char)]
      quicksort [] = []
      quicksort (x:xs) = quicksort [a | a <- xs, before a x] ++ [x] ++ quicksort [a | a <- xs, not $ before a x]
      listToString :: [((Int,Int),Char)] -> String -> Int -> IO String
      listToString list akku beforeY = 
        if Prelude.null list
        then return akku
        else do
          let character = (snd $ head list)
          let (x,y) = fst $ head list
          if y > beforeY
          then listToString (tail list) (character : '\n' : akku) y
          else listToString (tail list) (character : akku) y
      before :: ((Int,Int),Char) -> ((Int,Int),Char) -> Bool
      before ((a,b),_) ((c,d),_) = b < d || (b == d && a <= c)

deserialize :: String -> IO (TextAreaContent)
deserialize stringContent = do
    areaContent@((ChMap charMap _),_) <- TextAreaContent.init newX newY
    readStringListInEntryMap charMap lined (0,0)
    return areaContent
        where newX = maximum $ Prelude.map length lined
              newY = length lined
              lined = lines stringContent

readStringListInEntryMap _ [] _ = return ()
readStringListInEntryMap hmap (e:es) (x,y) = do
    readStringInEntryMap hmap e (0,y)
    readStringListInEntryMap hmap es (0,(y+1));

-- help function for deserialization
readStringInEntryMap _ [] _ = return ()
readStringInEntryMap hmap (s:ss) (x,y) = do
    entryMap <- readIORef hmap
    let entryMap = Map.insert (x,y) s entryMap
    writeIORef hmap entryMap
    readStringInEntryMap hmap ss (succ x,y)

-- quadruples the size of the TextAreaContent-Map
resize :: TextAreaContent -> IO ()
resize area@((ChMap charMap size),_)  = do
    (xm,ym) <- readIORef size
    expandXTextAreaN area (xm,ym) xm
    (xm,ym) <- readIORef size
    expandYTextAreaN area (xm,ym) ym

expandXTextAreaN area (oldX,oldY) n
  | n == 0 = return ()
  | otherwise = do
    expandXTextArea area (oldX,oldY)
    expandXTextAreaN area ((succ oldX),oldY) (n-1)

--Subfunction of expandXTextAreaN 
expandXTextArea area@((ChMap hMap size),_) (oldX,oldY)= do
  expandXTextAreaH area (oldX,oldY)
  (xmax,ymax) <- readIORef size
  writeIORef size (succ xmax,ymax)

--insert the new content at the end of a line
expandXTextAreaH area@(ChMap hMap _,CoMap colorMap _) (oldX,oldY) = 
  if oldY == 0
  then do
    hmap <- readIORef hMap
    cmap <- readIORef colorMap
    let hmap = Map.insert ((succ oldX),0) defaultChar hmap
    let cmap = Map.insert ((succ oldX),0) defaultColor cmap
    writeIORef hMap hmap
    writeIORef colorMap cmap
  else do
    hmap <- readIORef hMap
    cmap <- readIORef colorMap
    let hmap = Map.insert ((succ oldX),oldY) defaultChar hmap
    let cmap = Map.insert ((succ oldX),oldY) defaultColor cmap
    writeIORef hMap hmap
    writeIORef colorMap cmap
    expandXTextAreaH area (oldX,(pred oldY))

expandYTextAreaN area (oldX,oldY) n
  | n <= 0 = return ()
  | otherwise = do
    expandYTextArea area (oldX,oldY)
    expandYTextAreaN area (oldX,(succ oldY)) (n-1)

expandYTextArea area@(ChMap hMap size,_) (oldX,oldY)= do
  expandYTextAreaH area (oldX,oldY)
  (xmax,ymax) <- readIORef size
  writeIORef size (xmax,succ ymax)

--Insert a new line
expandYTextAreaH area@(ChMap hMap _,CoMap colorMap _) (oldX,oldY) = 
  if oldX == 0
  then do
    hmap <- readIORef hMap
    cmap <- readIORef colorMap
    let hmap = Map.insert (0,succ oldY) defaultChar hmap
    let cmap = Map.insert (0,succ oldY) defaultColor cmap
    writeIORef hMap hmap
    writeIORef colorMap cmap
  else do
    hmap <- readIORef hMap
    cmap <- readIORef colorMap
    let hmap = Map.insert (oldX,succ oldY) defaultChar hmap
    let cmap = Map.insert (oldX,succ oldY) defaultColor cmap
    writeIORef hMap hmap
    writeIORef colorMap cmap
    expandXTextAreaH area ((pred oldX),oldY)

-- sets given value for the specified coordinates
putValue :: TextAreaContent  -> (Int,Int) -> Char -> IO ()
putValue (ChMap charMap _,_) coord character = do
    hmap <- readIORef charMap
    let hmap = Map.insert coord character hmap
    writeIORef charMap hmap

putColor :: TextAreaContent -> (Int,Int) -> Color -> IO ()
putColor (_,CoMap colorMap _) coord color = do
    cmap <- readIORef colorMap
    let cmap = Map.insert coord color cmap
    writeIORef colorMap cmap

getCell :: TextAreaContent -> (Int,Int) -> IO (Char,Color)
getCell (ChMap charMap _,CoMap colorMap _) coord = do
    hmap <- readIORef charMap
    cmap <- readIORef colorMap
    let value = fromJust $ Map.lookup coord hmap
    let color = fromJust $ Map.lookup coord cmap
    return (value,color)

size :: TextAreaContent -> IO (Int,Int)
size (ChMap _ size, _) = do
    areaSize <- readIORef size
    return areaSize

