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
  defaultColor,

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
--data CharMap  = ChMap  (IORef (Map Position Char)) (IORef (Integer,Integer))
-- chMap is a Map(y (x coord char)) where y and x are coords
data CharMap  = ChMap  (IORef (Map Double (Map Double Char))) (IORef (Integer,Integer))

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
      hmap = fillCharMapWith (Map.insert 0 Map.empty Map.empty) defaultChar xD yD
      cmap = fillMapWith Map.empty defaultColor xD yD
      xD = fromInteger x :: Double
      yD = fromInteger y :: Double

--------------------
-- Methods

--fill a Map a (Map a b) with a specified content
fillCharMapWith :: Map.Map Double (Map.Map Double Char) --Map of characters at position y x
  -> Char --Content
  -> Double --Max x size
  -> Double --Max y size
  -> Map.Map Double (Map.Map Double Char)--Map with content at every y x coord (x*y operations)
fillCharMapWith charMap content xBound yBound =
  Prelude.foldl (\yMap yK -> 
    if (isNothing $ Map.lookup yK yMap) -- line Map nothing?
    then insert --insert the filled xMap into yMap
      yK 
      (Prelude.foldl (\xMap xK -> --foldl with empty map
        if (isNothing $ Map.lookup xK xMap) --check for char at x,y
        then Map.insert xK content xMap 
        else xMap)(Map.empty)[0..xBound])
      yMap
    else insert --insert the filled xMap into yMap
      yK
      (Prelude.foldl (\xMap xK -> --foldl with
        if (isNothing $ Map.lookup xK xMap) --check for char at x,y
        then Map.insert xK content xMap 
        else xMap)(fromJust $ Map.lookup yK yMap)[0..xBound])
       yMap
  )charMap [0..yBound]

--fills a map with a specified content
fillMapWith :: Map.Map Position a -> a -> Double -> Double -> Double -> Double -> Map.Map Position a
fillMapWith map content _ _ 0.0 0.0 = Map.insert (0.0,0.0) content map
fillMapWith map content xBound yBound 0.0 y = fillMapWith (Map.insert (0.0,y) content map) content xBound yBound xBound (pred y)
fillMapWith map content xBound yBound x y = fillMapWith (Map.insert (x,y) content map) content xBound yBound (pred x) y

-- | creates a string by a TextAreaContent
serialize :: TextAreaContent
  -> IO String
serialize areaContent = do
  let (ChMap hMap size) = charMap areaContent --get the CharMap pointer
  hmap <- readIORef hMap --get the CharMap
  (xMax,yMax) <- readIORef size --get the size of the CharMap
  --Fill the map with whitespaces
  let wMap = fillCharMapWith hmap ' ' (fromIntegral xMax) (fromIntegral yMax)
  {-1. Take a line map
    2. fold that map to a line
    3. reverse ist drop ' ' and reverse it again
    4. append \n
    5. goto 1. until Map.fold is finish
  -}
  return $ Map.fold (\lineMap code -> 
    ((reverse . dropWhile (== ' ') . reverse)
    (Map.fold (\char line ->line++[char]) "" lineMap))++['\n'] ) "" wMap
    
-- | creates a TextAreaContent by a string
deserialize :: String
  -> IO TextAreaContent
deserialize stringContent = do
  areaContent <- TextAreaContent.init newX newY
  let (ChMap hMap _) = charMap areaContent
  readStringListInEntryMap areaContent lined (0,0)
  return areaContent
    where
      newX = toInteger $ maximum $ Prelude.map length lined
      newY = toInteger $ length lined
      lined = lines stringContent

--subfunction for deserialization calls readStringInEntryMap for every line
readStringListInEntryMap :: TextAreaContent
  ->[String]--Lines
  -> Position
  -> IO()
readStringListInEntryMap _ [] _ = return ()
readStringListInEntryMap tac (e:es) (x,y) = do
  readStringInEntryMap tac e (0,y)
  readStringListInEntryMap tac es (0,y+1);

--subfunction for deserialization
readStringInEntryMap :: TextAreaContent
  -> String --Line
  -> Position
  -> IO()
readStringInEntryMap _ [] _ = return ()
readStringInEntryMap tac (s:ss) (x,y) = do
  putValue tac (x,y) s
  readStringInEntryMap tac ss (succ x,y)

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
  (xmax,ymax) <- readIORef size
  writeIORef size (succ xmax,ymax)
  print "before"
  print $ show(succ xmax,ymax)
  expandXTextAreaH areaContent (oldX,oldY)

--subfunction for expandXTextArea
expandXTextAreaH areaContent (oldX,oldY) = do
  if oldY == 0
  then do
    putColor areaContent (succ oldX,0.0) defaultColor
    putValue areaContent (succ oldX,0.0) defaultChar
  else do
    putValue areaContent (succ oldX,oldY) defaultChar
    putColor areaContent (succ oldX,oldY) defaultColor
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
  (xmax,ymax) <- readIORef size
  writeIORef size (xmax,succ ymax)
  expandYTextAreaH areaContent (oldX,oldY)
  
--subfunction for expandYTextArea
expandYTextAreaH areaContent (oldX,oldY) = do
  if oldX == 0
  then do
    putValue areaContent (0,succ oldY) defaultChar
    putColor areaContent (0, succ oldY) defaultColor
  else do
    putValue areaContent (oldX,succ oldY) defaultChar
    putColor areaContent (oldX, succ oldY) defaultColor
    expandXTextAreaH areaContent (pred oldX,oldY)

-- | sets the character for a cell identified by a coordinate
putValue :: TextAreaContent
  -> Position -- ^ coordinates of the required cell
  -> Char -- ^ character
  -> IO ()
putValue areaContent (x,y) character = do
  (xMax,yMax) <- TextAreaContent.size areaContent
  print "putValue"
  print $ show(x,y)
  print $ show(xMax,yMax)
  if (x > xMax || y > yMax)
  then error "putValue: Position out of Bounds"
  else do
    let 
      (ChMap hMap size) = charMap areaContent
      (CoMap cMap _) = colorMap areaContent
    valHMap <- readIORef hMap
    let lineMap = Map.lookup y valHMap
    if (isNothing lineMap) 
    then modifyIORef hMap (Map.insert y (Map.insert x character Map.empty))
    else modifyIORef hMap (Map.insert y (Map.insert x character $ fromJust lineMap))
    {-
    No need to set a a defaultColor, because it's only important to save highlighted chars.
    This improves performance in highlighter.
    cmap <- readIORef cMap
    let color = Map.lookup (x,y) cmap
    when (isNothing color) $ modifyIORef cMap (Map.insert (x,y) defaultColor)
    -}
-- | sets the color of a cell identified by 
putColor :: TextAreaContent
  -> Position -- ^ coordinates of the required cell
  -> RGBColor -- ^ color to put
  -> IO ()
putColor areaContent (x,y) color = do
  (xMax,yMax) <- TextAreaContent.size areaContent
  if (x > xMax || y > yMax)
  then error "putColor: Position out of Bounds"
  else do
    let (CoMap cMap _) = colorMap areaContent
    cmap <- readIORef cMap
    let cmap = Map.insert (x,y) color cmap
    writeIORef cMap cmap

-- | delets a cell
deleteCell :: TextAreaContent 
  -> Position
  -> IO (Bool)
deleteCell areaContent (x,y) = do
  let (ChMap hMap _) = charMap  areaContent
  let (CoMap cMap _) = colorMap areaContent
  (xMax,yMax) <- TextAreaContent.size areaContent
  if (x > xMax || y > yMax)
  then return False
  else do
    let delete = (\m -> Map.delete x (fromJust $ Map.lookup y m))
    modifyIORef hMap (\m -> Map.insert y (delete m) m)
    modifyIORef cMap (Map.delete (x,y))
    return True

-- | returns the character and the color of a cell
getCell :: TextAreaContent 
  -> Position -- ^ coordinates of the required cell
  -> IO (Maybe (Char,RGBColor))
getCell areaContent (x,y) = do
  let (ChMap hMap _) = charMap areaContent
  let (CoMap cMap _) = colorMap areaContent
  hmap <- readIORef hMap
  cmap <- readIORef cMap
  let valMap =  Map.lookup y hmap
  let mayColor =  Map.lookup (x,y) cmap
  case (isNothing valMap) of
    True -> return Nothing
    _ -> do
      let mayValue = Map.lookup x $ fromJust valMap
      case (isNothing mayValue) of
       True -> return Nothing
       _ -> 
         case (isNothing mayColor) of
          True -> return $ Just (fromJust mayValue, defaultColor)
          _ -> return $ Just (fromJust mayValue, fromJust mayColor)

generateContentList :: TextAreaContent
  -> (Position -> Bool)
  -> IO (ContentList)
generateContentList areaContent function = do
  let (ChMap hMap _) = charMap areaContent
  hmap <- readIORef hMap
  -- make the old map format from ChMap
  let 
    foldIns :: Double -> Map.Map Double Char -> Map.Map Position Char -> Map.Map Position Char
    foldIns = 
      (\y lineMap posCharMap ->
        Map.foldWithKey (\x val posCharMap ->
          Map.insert (x,y) val posCharMap
        ) posCharMap lineMap
      ) 
    mapPosChar = Map.foldWithKey foldIns Map.empty hmap
    list = Map.toList $ Map.filterWithKey (\coord _ -> function coord) mapPosChar
  return list

-- | returns the size of a TextAreaContent.
size :: TextAreaContent
  -> IO (Position) -- ^ size of the TextAreaContent
size areaContent = do 
  let (ChMap _ size) = charMap areaContent
  (x,y) <- readIORef size
  return (fromInteger x :: Double, fromInteger y :: Double)
