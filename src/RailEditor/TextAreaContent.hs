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
-- * Detail
--
-- | 'TextAreaContent' is a model to be used in combination with 'TextArea' as view.

-- * Types
  TextAreaContent,
  Position,
  ContentList,
  RGBColor(RGBColor),

-- * Constructors
  TextAreaContent.init,   -- initializes both data structures

-- * Constants
  black,
  white,
  gold,
  green,
  blue,
  red,

-- * Methods
  serialize,
  deserialize,
  putValue,
  putColor,
  getCell,
  deleteCell,
  TextAreaContent.size,
  generateContentList
  ) where

import Graphics.UI.Gtk
import Data.Map as Map
import Data.IORef
import Data.Maybe
import Control.Monad

data RGBColor = RGBColor Double Double Double
data ColorMap = CoMap  (IORef (Map Position RGBColor)) (IORef (Integer,Integer))
data CharMap  = ChMap  (IORef (Map Position Char)) (IORef (Integer,Integer))

data TextAreaContent = TAC {charMap :: CharMap, colorMap :: ColorMap}
type Position = (Double,Double)
type ContentList = [(Position,Char)]

-- Constants
red   = RGBColor 1.0                  0                   0
blue  = RGBColor 3.781185626001373e-2 0.21072709239337759 0.965316243228809
green = RGBColor 5.145342183566033e-2 0.9518730449378194  8.746471351186388e-2
gold  = RGBColor 1.0                  0.46433203631647213 0.0
black = RGBColor 0                    0                   0
white = RGBColor 1.0                  1.0                 1.0

defaultColor = black
defaultChar = ' '


--------------------
-- Constructors

-- | creates a new TextAreaContent
init :: Integer -- ^ x-size
  -> Integer -- ^ y-size
  -> IO TextAreaContent
init x y = do
  size <- newIORef (x,y)
  hmapR <- newIORef Map.empty
  cmapR <- newIORef Map.empty
  let cMap = CoMap cmapR size
  let hMap = ChMap hmapR size
  return $ TAC hMap cMap
    where
      hmap = fillMapWith Map.empty defaultChar xD yD xD yD
      cmap = fillMapWith Map.empty defaultColor xD yD xD yD
      xD = fromInteger x :: Double
      yD = fromInteger y :: Double

--------------------
-- Methods

--fills a map with a specified content
fillMapWith :: Map.Map Position a -> a -> Double -> Double -> Double -> Double -> Map.Map Position a
fillMapWith map content _ _ 0.0 0.0 = Map.insert (0.0,0.0) content map
fillMapWith map content xBound yBound 0.0 y = fillMapWith (Map.insert (0.0,y) content map) content xBound yBound xBound (pred y)
fillMapWith map content xBound yBound x y = fillMapWith (Map.insert (x,y) content map) content xBound yBound (pred x) y

-- | creates a string by a TextAreaContent
serialize :: TextAreaContent
  -> IO String
serialize areaContent = do
  let (ChMap hMap size) = charMap areaContent
  hmap <- readIORef hMap
  let list = toList hmap
  let sortedList = quicksort list
  result <- listToString sortedList [] 0
  let rightOrder = unlines $ Prelude.map (reverse . dropWhile (== ' ') . reverse) (lines $ reverse result)
  return rightOrder
    where
      quicksort :: [(Position,Char)] -> [(Position,Char)]
      quicksort [] = []
      quicksort (x:xs) = quicksort [a | a <- xs, before a x] ++ [x] ++ quicksort [a | a <- xs, not $ before a x]
      listToString :: [(Position,Char)] -> String -> Double -> IO String
      listToString list akku beforeY = 
        if Prelude.null list
        then return akku
        else do
          let character = snd $ head list
          let (x,y) = fst $ head list
          if y > beforeY
          then listToString (tail list) (character : '\n' : akku) y
          else listToString (tail list) (character : akku) y
      before :: (Position,Char) -> (Position,Char) -> Bool
      before ((a,b),_) ((c,d),_) = b < d || (b == d && a <= c)

-- | creates a TextAreaContent by a string
deserialize :: String
  -> IO TextAreaContent
deserialize stringContent = do
  areaContent <- TextAreaContent.init newX newY
  let (ChMap hMap _) = charMap areaContent
  readStringListInEntryMap hMap lined (0,0)
  return areaContent
    where
      newX = toInteger $ maximum $ Prelude.map length lined
      newY = toInteger $ length lined
      lined = lines stringContent

--subfunction for deserialization
readStringListInEntryMap _ [] _ = return ()
readStringListInEntryMap hmap (e:es) (x,y) = do
  readStringInEntryMap hmap e (0,y)
  readStringListInEntryMap hmap es (0,y+1);

--subfunction for deserialization
readStringInEntryMap _ [] _ = return ()
readStringInEntryMap hmap (s:ss) (x,y) = do
  entryMap <- readIORef hmap
  let entryMap = Map.insert (x,y) s entryMap
  writeIORef hmap entryMap
  readStringInEntryMap hmap ss (succ x,y)

--quadruples the size of the TextAreaContent
resize :: TextAreaContent -> IO ()
resize areaContent  = do
  let (ChMap _ size) = charMap areaContent
  (xm,ym) <- readIORef size
  expandXTextAreaN areaContent (fromInteger xm :: Double,fromInteger ym :: Double) xm
  (xm,ym) <- readIORef size
  expandYTextAreaN areaContent (fromInteger xm :: Double,fromInteger ym :: Double) ym

--subfunction for resize
expandXTextAreaN area (oldX,oldY) n
  | n == 0 = return ()
  | otherwise = do
    expandXTextArea area (oldX,oldY)
    expandXTextAreaN area (succ oldX,oldY) (n-1)

--subfunction for expandXTextAreaN
expandXTextArea areaContent (oldX,oldY)= do
  let (ChMap _ size) = charMap areaContent
  expandXTextAreaH areaContent (oldX,oldY)
  (xmax,ymax) <- readIORef size
  writeIORef size (succ xmax,ymax)

--subfunction for expandXTextArea
expandXTextAreaH areaContent (oldX,oldY) = do
  let (ChMap hMap _) = charMap areaContent
  let (CoMap cMap _) = colorMap areaContent
  if oldY == 0
  then do
    hmap <- readIORef hMap
    cmap <- readIORef cMap
    let hmap = Map.insert (succ oldX,0.0) defaultChar hmap
    let cmap = Map.insert (succ oldX,0.0) defaultColor cmap
    writeIORef hMap hmap
    writeIORef cMap cmap
  else do
    hmap <- readIORef hMap
    cmap <- readIORef cMap
    let hmap = Map.insert (succ oldX,oldY) defaultChar hmap
    let cmap = Map.insert (succ oldX,oldY) defaultColor cmap
    writeIORef hMap hmap
    writeIORef cMap cmap
    expandXTextAreaH areaContent (oldX,pred oldY)

--subfunction for resize
expandYTextAreaN areaContent (oldX,oldY) n
  | n <= 0 = return ()
  | otherwise = do
    expandYTextArea areaContent (oldX,oldY)
    expandYTextAreaN areaContent (oldX,succ oldY) (n-1)

--subfunction for expandYTextAreaN
expandYTextArea areaContent (oldX,oldY)= do
  let (ChMap _ size) = charMap areaContent
  expandYTextAreaH areaContent (oldX,oldY)
  (xmax,ymax) <- readIORef size
  writeIORef size (xmax,succ ymax)

--subfunction for expandYTextArea
expandYTextAreaH areaContent (oldX,oldY) = do
  let (ChMap hMap _) = charMap areaContent
  let (CoMap cMap _) = colorMap areaContent
  if oldX == 0
  then do
    hmap <- readIORef hMap
    cmap <- readIORef cMap
    let hmap = Map.insert (0,succ oldY) defaultChar hmap
    let cmap = Map.insert (0,succ oldY) defaultColor cmap
    writeIORef hMap hmap
    writeIORef cMap cmap
  else do
    hmap <- readIORef hMap
    cmap <- readIORef cMap
    let hmap = Map.insert (oldX,succ oldY) defaultChar hmap
    let cmap = Map.insert (oldX,succ oldY) defaultColor cmap
    writeIORef hMap hmap
    writeIORef cMap cmap
    expandXTextAreaH areaContent (pred oldX,oldY)

-- | sets the character for a cell identified by a coordinate
putValue :: TextAreaContent
  -> Position -- ^ coordinates of the required cell
  -> Char -- ^ character
  -> IO ()
putValue areaContent coord character = do
  let (ChMap hMap _) = charMap areaContent
  let (CoMap cMap _) = colorMap areaContent
  modifyIORef hMap (Map.insert coord character)
  cmap <- readIORef cMap
  let color = Map.lookup coord cmap
  when (isNothing color) $ modifyIORef cMap (Map.insert coord defaultColor)

-- | sets the color of a cell identified by 
putColor :: TextAreaContent
  -> Position -- ^ coordinates of the required cell
  -> RGBColor -- ^ color to put
  -> IO ()
putColor areaContent coord color = do
  let (CoMap cMap _) = colorMap areaContent
  cmap <- readIORef cMap
  let cmap = Map.insert coord color cmap
  writeIORef cMap cmap

-- | delets a cell
deleteCell :: TextAreaContent 
  -> Position
  -> IO (Bool)
deleteCell areaContent coord = do
  let (ChMap hMap _) = charMap  areaContent
  let (CoMap cMap _) = colorMap areaContent
  let succ = True
  hmap <- readIORef hMap
  cmap <- readIORef cMap
  when (isNothing $ Map.lookup coord cmap) $ do
    let succ = False
    return ()
  when (isNothing $ Map.lookup coord hmap) $ do
    let succ = False
    return ()
  modifyIORef hMap (Map.delete coord)
  modifyIORef cMap (Map.delete coord)
  return succ

-- | returns the character and the color of a cell
getCell :: TextAreaContent 
  -> Position -- ^ coordinates of the required cell
  -> IO (Maybe (Char,RGBColor))
getCell areaContent coord = do
  let (ChMap hMap _) = charMap areaContent
  let (CoMap cMap _) = colorMap areaContent
  hmap <- readIORef hMap
  cmap <- readIORef cMap
  let mayValue =  Map.lookup coord hmap
  let mayColor =  Map.lookup coord cmap
  if (isNothing mayValue) 
  then return Nothing
  else if (isNothing mayColor) 
       then return Nothing
       else return $ Just (fromJust mayValue, fromJust mayColor)

generateContentList :: TextAreaContent
  -> (Position -> Bool)
  -> IO (ContentList)
generateContentList areaContent function = do
  let (ChMap hMap _) = charMap areaContent
  hmap <- readIORef hMap
  let list = Map.toList $ Map.filterWithKey (\coord _ -> function coord) hmap
  return list

-- | returns the size of a TextAreaContent.
size :: TextAreaContent
  -> IO (Position) -- ^ size of the TextAreaContent
size areaContent = do 
  let (ChMap _ size) = charMap areaContent
  (x,y) <- readIORef size
  return (fromInteger x :: Double, fromInteger y :: Double)
