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
  Coord,
  ContentList,
  RGBColor(RGBColor),
  TextAreaContent.Action(Remove, Insert, Replace, Concat),
  ActionQueue,

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
  defaultChar,

-- * Methods
  serialize,
  deserialize,
  putValue,
  putColor,
  putCell,
  getPositionedGrid,
  getOccupiedPositions,
  getCell,
  isEmptyLine,
  deleteCell,
  eqPos,
  deleteColors,
  TextAreaContent.size,
  generateContentList,
  redoQueue,
  undoQueue
  ) where

import Graphics.UI.Gtk
import qualified InterfaceDT as IDT
import Data.Map as Map
import Data.IORef
import Data.Maybe
import qualified Data.List as List
import Control.Monad

data RGBColor = RGBColor Double Double Double deriving Show
data ColorMap = CoMap  (IORef (Map Position RGBColor)) (IORef (Coord,Coord))
-- chMap is a Map(y (x coord char)) where y and x are coords
data CharMap  = ChMap  (IORef (Map Coord (Map Coord Char))) (IORef (Coord,Coord))
{-
  This is the dataType to remember current occupied cells
  Its is important to increase the speed of redrawContent from theta(n²)
  to O(n²). In avarage cases it should be much faster.
-}
data ContentPositions = ConPos (IORef ([Position]))
-- types for undoredo
data Action = Remove String | Insert String | Replace String String | Concat (TextAreaContent.Action, Position) (TextAreaContent.Action, Position) deriving Show
type ActionQueue = [(TextAreaContent.Action, Position)]

data TextAreaContent = 
  TAC {
    charMap :: CharMap,
    colorMap :: ColorMap,
    conPos :: ContentPositions,
    undoQueue :: IORef ActionQueue,
    redoQueue :: IORef ActionQueue 
  }
  
type Coord = Int
type Position = (Coord,Coord)
type ContentList = [(Position,Char)]

-- Constants
red   = RGBColor 1.0                  0                   0
blue  = RGBColor 3.781185626001373e-2 0.21072709239337759 0.965316243228809
green = RGBColor 5.145342183566033e-2 0.9518730449378194  8.746471351186388e-2
gold  = RGBColor 1.0                  0.46433203631647213 0.0
black = RGBColor 0                    0                   0
white = RGBColor 1.0                  1.0                 1.0

defaultColor = red
defaultChar = ' '


--------------------
-- Constructors

-- | creates a new TextAreaContent
init :: Coord -- ^ x-size
  -> Coord -- ^ y-size
  -> IO TextAreaContent
init x y = do
  size <- newIORef (x,y)
  hmapR <- newIORef Map.empty
  cmapR <- newIORef Map.empty
  contPL <- newIORef []
  undoQ <- newIORef []
  redoQ <- newIORef []
  let 
    cMap = CoMap cmapR size
    hMap = ChMap hmapR size
    contP = ConPos contPL
  return $ TAC hMap cMap contP undoQ redoQ

--------------------
-- Methods

--To compare Positions
eqPos :: Position -> Position -> Bool
eqPos (x,y) (u,v) = (x==u)&&(y==v)

--fill a Map a (Map a b) with a specified content
fillCharMapWith :: Map.Map Coord (Map.Map Coord Char) --Map of characters at position y x
  -> Char --Content
  -> Position -- size
  -> Map.Map Coord (Map.Map Coord Char)--Map with content at every y x coord (x*y operations)
fillCharMapWith charMap content (xBound,yBound) =
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
fillMapWith :: Map.Map Position a -> a -> Coord -> Coord -> Coord -> Coord -> Map.Map Position a
fillMapWith map content _ _ 0 0 = Map.insert (0,0) content map
fillMapWith map content xBound yBound 0 y = fillMapWith (Map.insert (0,y) content map) content xBound yBound xBound (pred y)
fillMapWith map content xBound yBound x y = fillMapWith (Map.insert (x,y) content map) content xBound yBound (pred x) y

-- | creates a string by a TextAreaContent
serialize :: TextAreaContent
  -> IO String
serialize areaContent = do
  let (ChMap hMap size) = charMap areaContent --get the CharMap pointer
  hmap <- readIORef hMap --get the CharMap
  (xMax,yMax) <- readIORef size --get the size of the CharMap
  --Fill the map with whitespaces
  let 
    wMap = fillCharMapWith hmap ' ' (xMax,yMax)
    listOfLines = assocs wMap
  foldM (\code (y,lineMap) -> do
      let lineList = assocs lineMap
      l <- foldM (\line (x,char) -> return $ line++[char]) "" lineList
      let cleanLine = ((reverse . dropWhile (== ' ') . reverse) l)++['\n']
      return $ code++cleanLine
    ) "" listOfLines
   
-- | creates a TextAreaContent by a string
deserialize :: String
  -> IO TextAreaContent
deserialize stringContent = do
  areaContent <- TextAreaContent.init newX newY
  --readStringListInEntryMap areaContent lined (0,0) 
  foldM (\y line -> do
    foldM (\x char -> do 
        putValue areaContent (x,y) char
        return $ x+1
      ) 0 line 
    return $ y+1
    ) 0 lined 
  return areaContent
  where
    newX = maximum $ Prelude.map length lined
    newY = length lined
    lined = lines stringContent

--set the size(x,y) to (x+1,y+1)
resize :: TextAreaContent -> Position-> IO ()
resize areaContent (xm,ym) = do
  let 
    (ChMap charMapIORef charSize) = charMap areaContent
    (CoMap colorMapIORef colorSize) = colorMap areaContent
  writeIORef charSize (xm,ym)
  modifyIORef' colorSize (\_ -> (xm,ym))

-- | sets the character for a cell identified by a coordinate
-- If the coords are out of bounds the function resizes the TAC.
putValue :: TextAreaContent
  -> Position -- ^ coordinates of the required cell
  -> Char -- ^ character
  -> IO ()
putValue areaContent (x,y) character = do
  (xMax,yMax) <- TextAreaContent.size areaContent
  if (y > yMax || x > xMax)
  then do 
    resize areaContent (xMax + (abs $ xMax-x),yMax + (abs $ yMax-y))
    putValue areaContent (x,y) character
  else do
    putPos areaContent (x,y)
    let 
      (ChMap hMap size) = charMap areaContent
      (CoMap cMap _) = colorMap areaContent
    valHMap <- readIORef hMap
    let lineMap = Map.lookup y valHMap
    if (isNothing lineMap) 
    then modifyIORef' hMap (Map.insert y (Map.insert x character Map.empty))
    else modifyIORef' hMap (Map.insert y (Map.insert x character $ fromJust lineMap))

-- | sets the color of a cell identified by 
-- If the coords are out of bounds the function resizes the TAC.
putColor :: TextAreaContent
  -> Position -- ^ coordinates of the required cell
  -> RGBColor -- ^ color to put
  -> IO ()
putColor areaContent (x,y) color = do
  (xMax,yMax) <- TextAreaContent.size areaContent
  if (x > xMax || y > yMax)
  then do
    resize areaContent (xMax + (abs $ xMax-x),yMax + (abs $ yMax-y))
    putColor areaContent (x,y) color
  else do
    let (CoMap cMap _) = colorMap areaContent
    modifyIORef' cMap (\cmap -> Map.insert (x,y) color cmap)

deleteColors :: TextAreaContent -> IO ()
deleteColors tac = do
  let(CoMap cMap _) = colorMap tac
  modifyIORef cMap (\_ -> Map.empty)

-- | Returns the occupied positions in TAC
getOccupiedPositions :: TextAreaContent
  -> IO([Position])
getOccupiedPositions tac = readIORef conPosIORef
  where (ConPos conPosIORef) = conPos tac
  
-- | puts a position to the list of occupied
putPos :: TextAreaContent
  -> Position
  -> IO()
putPos tac pos = do
 let (ConPos conPosIORef) = conPos tac
 modifyIORef conPosIORef (\conPL -> conPL++[pos])
 
-- | delets a position from list of occupied positions 
deletePos :: TextAreaContent
  -> Position
  -> IO ()
deletePos tac pos = do
  let (ConPos conPosIORef) = conPos tac
  modifyIORef conPosIORef (\conPL -> List.delete pos conPL)
 
-- / sets a cell
putCell :: TextAreaContent
  -> Position -- ^ coordinates of the required cell
  -> (Char,RGBColor) -- ^ char and color to put
  -> IO ()
putCell areaContent coord (char,color) = do
  putColor areaContent coord color
  putValue areaContent coord char

-- | delets a cell
deleteCell :: TextAreaContent 
  -> Position
  -> IO (Bool)
deleteCell areaContent (x,y) = do
  let 
    (ChMap hMap _) = charMap  areaContent
    (CoMap cMap _) = colorMap areaContent
    (ConPos conPosIORef) = conPos areaContent
  (xMax,yMax) <- TextAreaContent.size areaContent
  if (x > xMax || y > yMax)
  then return False
  else do
    let delete = (\m -> Map.delete x (fromJust $ Map.lookup y m))
    deletePos areaContent (x,y)
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
  let color =  Map.findWithDefault defaultColor (x,y) cmap --now lazy
  case (isNothing valMap) of
    True -> return Nothing
    _ -> do
      let mayValue = Map.lookup x $ fromJust valMap
      case (isNothing mayValue) of
       True -> return Nothing
       _ -> return $ Just (fromJust mayValue, color)
       
-- / checks if line is empty
isEmptyLine :: TextAreaContent
  -> Coord
  -> IO(Bool)
isEmptyLine areaContent line = do
  let (ChMap hMap _) = charMap areaContent
  hmap <- readIORef hMap
  let val =  Map.lookup line hmap
  if val==Nothing
  then return True
  else return False

generateContentList :: TextAreaContent
  -> (Position -> Bool)
  -> IO (ContentList)
generateContentList areaContent function = do
  let (ChMap hMap _) = charMap areaContent
  hmap <- readIORef hMap
  -- make the old map format from ChMap
  let 
    foldIns :: Coord -> Map.Map Coord Char -> Map.Map Position Char -> Map.Map Position Char
    foldIns = 
      (\y lineMap posCharMap ->
        Map.foldWithKey (\x val posCharMap ->
          Map.insert (x,y) val posCharMap
        ) posCharMap lineMap
      ) 
    mapPosChar = Map.foldWithKey foldIns Map.empty hmap
    list = Map.toList $ Map.filterWithKey (\coord _ -> function coord) mapPosChar
  return list

{-This function is important for highlighting it returns the datatype
  which Lexer needs to lex. -}
getPositionedGrid :: TextAreaContent -> IO(IDT.PreProc2Lexer)
getPositionedGrid areaContent = do
  let (ChMap hMap hSize) = charMap areaContent
  s@(xMax,yMax) <- readIORef hSize
  hmap <- readIORef hMap
  pGrid <- buildPosGrid ([],0) (assocs hmap)
  return $ IDT.IPL (fst pGrid)
  where
    buildPosGrid :: ([IDT.PositionedGrid],Int) -> [(Int,(Map.Map Int Char))] -> IO(([IDT.PositionedGrid],Int))
    buildPosGrid a b = foldM  
      (\(grids,offset) (y,line) -> do
        let mayDollar = Map.lookup 0 line 
        if (isNothing mayDollar)
        then return $ ((insertWhenFct grids line y offset),offset)
        else 
          if ((fromJust mayDollar) == '$')
          then return $ (grids++[((Map.insert 0 line Map.empty),y)],y)
          else return $ ((insertWhenFct grids line y offset),offset)
      ) a b
    
    insertWhenFct :: [IDT.PositionedGrid] -> (Map.Map Int Char) -> Int -> Int -> [IDT.PositionedGrid]
    insertWhenFct x line y  offset 
      | x == [] || line == Map.empty = x
      | otherwise = x++[(Map.insert (y-offset) line (fst (last x)), (snd (last x)))]
  
-- | returns the size of a TextAreaContent.
size :: TextAreaContent
  -> IO (Position) -- ^ size of the TextAreaContent
size areaContent = do 
  let (ChMap _ size) = charMap areaContent
  s <- readIORef size
  return s
