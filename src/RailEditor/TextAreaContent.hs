{- |
Module      :  TextAreaContent.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann, Benjamin Kodera (c)
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
  Direction,
  Coord,
  RGBColor(RGBColor),
  TextAreaContent.Action(Remove, Insert, RemoveLine, InsertLine, Replace, Concat, DoNothing),
  ActionQueue,
  RailType(RailString, RailList, RailLambda),
  InterpreterContext(IC), dataStack, funcStack, breakMap, inputOffset, curIPPos, railFlags,
  RailFlag(Interpret,Step,Blocked,EOFWait),

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
  putDirection,
  getPositionedGrid,
  getCell,
  getDirection,
  isEmptyLine,
  findLastChar, --needed for KeyHandler
  deleteCell,
  eqPos,
  deleteColors,
  TextAreaContent.size,
  redoQueue,
  undoQueue,
  context,
  buffer,
  getSelectedPositons,
  getPositons,
  getPositonsFrom,
  setClipboard,
  getClipboard
  ) where

import qualified InterfaceDT as IDT
import Data.Map as Map
import Data.IORef
import Data.Maybe
import qualified Data.List as List
import Control.Monad
import qualified Lexer
import Graphics.UI.Gtk as Gtk

data RGBColor = RGBColor Double Double Double deriving Show
data ColorMap = CoMap  (IORef (Map Position RGBColor)) (IORef (Coord,Coord))
-- chMap is a Map(y (x coord char)) where y and x are coords
data CharMap  = ChMap  (IORef (Map Coord (Map Coord (Char,Bool)))) (IORef (Coord,Coord))

-- types for undoredo
data Action = Remove [(Char,Bool)] | Insert [(Char,Bool)] | Replace [(Char,Bool)] [(Char,Bool)] | RemoveLine | InsertLine | Concat (TextAreaContent.Action, Position) (TextAreaContent.Action, Position) | DoNothing deriving Show
type ActionQueue = [(TextAreaContent.Action, Position)]

-- types for interpreter
data RailType = RailString String | RailList [RailType] | RailLambda String Lexer.IP (Map.Map String RailType) deriving (Eq)

data RailFlag = Interpret | Step | Blocked | EOFWait deriving (Eq)

instance Show RailType
 where
  show (RailString string) = string
  show (RailList list) = show list
  show (RailLambda string ip _) = string ++ show (Lexer.posx ip, Lexer.posy ip)
data InterpreterContext = 
  IC {
    dataStack :: [RailType],
    funcStack :: [(String, Lexer.IP, Map.Map String RailType)],
    breakMap :: Map Position Bool,
    inputOffset :: Int,
    curIPPos :: Position,
    railFlags :: [RailFlag]
  }

data TextAreaContent = 
  TAC {
    charMap :: CharMap,
    colorMap :: ColorMap,
    undoQueue :: IORef ActionQueue,
    redoQueue :: IORef ActionQueue,
    railDirection :: IORef Direction,
    context :: IORef InterpreterContext,
    buffer :: (Gtk.TextBuffer,Gtk.TextBuffer),
    clipboard :: IORef [(Position,(Char,Bool))]
  }
  
type Coord = Int
type Position = (Coord,Coord)
type Direction = (Coord,Coord)

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
  -> Gtk.TextBuffer
  -> Gtk.TextBuffer
  -> IO TextAreaContent
init x y inputBuffer outputBuffer= do
  size <- newIORef (x,y)
  hmapR <- newIORef Map.empty
  cmapR <- newIORef Map.empty
  undoQ <- newIORef []
  redoQ <- newIORef []
  dir <- newIORef (1,0)
  cont <- newIORef $ IC [] [] Map.empty 0 (-1, -1) []
  clipb <- newIORef []
  let 
    cMap = CoMap cmapR size
    hMap = ChMap hmapR size
  return $ TAC hMap cMap undoQ redoQ dir cont (inputBuffer,outputBuffer) clipb

--------------------
-- Methods

--To compare Positions
eqPos :: Position -> Position -> Bool
eqPos (x,y) (u,v) = (x==u)&&(y==v)

--fill a Map a (Map a b) with a specified content
fillCharMapWith :: Map.Map Coord (Map.Map Coord (Char,Bool)) --Map of content at position y x
  -> (Char,Bool) --Content
  -> Position -- size
  -> Map.Map Coord (Map.Map Coord (Char,Bool))--Map with content at every y x coord (x*y operations)
fillCharMapWith charMap content (xBound,yBound) =
  Prelude.foldl (\yMap yK -> 
    if isNothing $ Map.lookup yK yMap -- line Map nothing?
    then insert --insert the filled xMap into yMap
      yK 
      (Prelude.foldl (\xMap xK -> --foldl with empty map
        if isNothing $ Map.lookup xK xMap --check for char at x,y
        then Map.insert xK content xMap 
        else xMap) Map.empty [0..xBound])
      yMap
    else insert --insert the filled xMap into yMap
      yK
      (Prelude.foldl (\xMap xK -> --foldl with
        if isNothing $ Map.lookup xK xMap --check for char at x,y
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
    wMap = fillCharMapWith hmap (' ',False) (xMax,yMax)
    listOfLines = assocs wMap
  foldM (\code (y,lineMap) -> do
      let lineList = assocs lineMap
      l <- foldM (\line (x,(char,_)) -> return $ line++[char]) "" lineList
      let cleanLine = (reverse . dropWhile (== ' ') . reverse) l ++ "\n"
      return $ code++cleanLine
    ) "" listOfLines
   
-- | creates a TextAreaContent by a string
deserialize :: String
  -> Gtk.TextBuffer
  -> Gtk.TextBuffer
  -> IO TextAreaContent
deserialize stringContent inputB outputB = do
  areaContent <- TextAreaContent.init newX newY inputB outputB
  --readStringListInEntryMap areaContent lined (0,0) 
  foldM_ (\y line -> do
    foldM_ (\x char -> do 
        putValue areaContent (x,y) (char,False)
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
  writeIORef colorSize (xm,ym)

-- | sets the character for a cell identified by a coordinate
-- If the coords are out of bounds the function resizes the TAC.
putValue :: TextAreaContent
  -> Position -- ^ coordinates of the required cell
  -> (Char,Bool) -- ^ character and selection state
  -> IO ()
putValue areaContent (x,y) content = do
  (xMax,yMax) <- TextAreaContent.size areaContent
  if y > yMax || x > xMax
  then do 
    resize areaContent (xMax + abs (xMax-x),yMax + abs (yMax-y))
    putValue areaContent (x,y) content
  else do
    let 
      (ChMap hMap size) = charMap areaContent
      (CoMap cMap _) = colorMap areaContent
    valHMap <- readIORef hMap
    let lineMap = Map.lookup y valHMap
    modifyIORef' hMap $ Map.insert y $ maybe (insert x content empty) (insert x content) lineMap

-- | sets the color of a cell identified by 
-- If the coords are out of bounds the function resizes the TAC.
putColor :: TextAreaContent
  -> Position -- ^ coordinates of the required cell
  -> RGBColor -- ^ color to put
  -> IO ()
putColor areaContent (x,y) color = do
  (xMax,yMax) <- TextAreaContent.size areaContent
  if x > xMax || y > yMax
  then do
    resize areaContent (xMax + abs (xMax-x),yMax + abs (yMax-y))
    putColor areaContent (x,y) color
  else do
    let (CoMap cMap _) = colorMap areaContent
    modifyIORef' cMap $ Map.insert (x,y) color

deleteColors :: TextAreaContent -> IO ()
deleteColors tac = do
  let(CoMap cMap _) = colorMap tac
  writeIORef cMap Map.empty

-- / sets a cell
putCell :: TextAreaContent
  -> Position -- ^ coordinates of the required cell
  -> ((Char,Bool),RGBColor) -- ^ content and color to put
  -> IO ()
putCell areaContent coord (content,color) = do
  putColor areaContent coord color
  putValue areaContent coord content

-- | setting input direction
putDirection :: TextAreaContent
  -> Direction
  -> IO ()
putDirection tac dir = do
  let dirTac = railDirection tac
  modifyIORef dirTac $ const dir

-- | getting input direction
getDirection :: TextAreaContent
  -> IO Direction
getDirection tac = do
  let dirTac = railDirection tac
  readIORef dirTac

-- | delets a cell
deleteCell :: TextAreaContent 
  -> Position
  -> IO Bool
deleteCell areaContent (x,y) = do
  let 
    (ChMap hMap _) = charMap  areaContent
    (CoMap cMap _) = colorMap areaContent
  (xMax,yMax) <- TextAreaContent.size areaContent
  if x > xMax || y > yMax
  then return False
  else do
    modifyIORef hMap (\m -> del m (x,y))
    modifyIORef cMap (Map.delete (x,y))
    return True
  where
    delMap :: Map.Map Coord (Map.Map Coord (Char,Bool)) -> Position -> Map.Map Coord (Char,Bool)
    delMap m (x,y) = maybe Map.empty (Map.delete x) (Map.lookup y m)
    del :: Map.Map Coord (Map.Map Coord (Char,Bool)) -> Position -> Map.Map Coord (Map.Map Coord (Char,Bool))
    del m (x,y)
      | delMap m (x,y) == Map.empty = Map.delete y m
      | otherwise = Map.insert y (delMap m (x,y)) m
     
-- | returns the character and the color of a cell
getCell :: TextAreaContent 
  -> Position -- ^ coordinates of the required cell
  -> IO (Maybe ((Char,Bool),RGBColor))
getCell areaContent (x,y) = do
  let (ChMap hMap _) = charMap areaContent
  let (CoMap cMap _) = colorMap areaContent
  hmap <- readIORef hMap
  cmap <- readIORef cMap
  let valMap =  Map.lookup y hmap
  let color =  Map.findWithDefault defaultColor (x,y) cmap --now lazy
  return $ if isNothing valMap then Nothing else do
      let mayValue = Map.lookup x $ fromJust valMap
      if isNothing mayValue then Nothing else Just (fromJust mayValue, color)
       
-- / checks if line is empty
isEmptyLine :: TextAreaContent
  -> Coord
  -> IO Bool
isEmptyLine areaContent line = do
  let (ChMap hMap _) = charMap areaContent
  hmap <- readIORef hMap
  return $ isNothing $ Map.lookup line hmap

{-This function is important for highlighting it returns the datatype
  which Lexer needs to lex. -}
getPositionedGrid :: TextAreaContent -> IO IDT.PreProc2Lexer
getPositionedGrid areaContent = do
  let (ChMap hMap hSize) = charMap areaContent
  s@(xMax,yMax) <- readIORef hSize
  hmap <- readIORef hMap
  pGrid <- buildPosGrid ([],0) (assocs hmap)
  return $ IDT.IPL (Prelude.map (maximize (max xMax yMax)) $ fst pGrid)
  where
    maximize :: Int -> IDT.PositionedGrid -> IDT.PositionedGrid
    maximize msize (grid, offset) = (Map.update updatefirst 0 grid, offset)
      where
        updatefirst line = Just $ Map.union line emptymap
        emptymap = fromList $ zip [0..msize] (repeat ' ')
    buildPosGrid :: ([IDT.PositionedGrid],Int) -> [(Int,Map.Map Int (Char,Bool))] -> IO ([IDT.PositionedGrid],Int)
    buildPosGrid = foldM  
      (\(grids,offset) (y,line) -> do
        let mayDollar = Map.lookup 0 line 
            simplifiedLine = Map.map fst line
        return $ 
          if isNothing mayDollar
          then (insertWhenFct grids simplifiedLine y offset,offset)
          else
            if fst (fromJust mayDollar) == '$'
            then (grids ++ [(Map.insert 0 simplifiedLine Map.empty, y)], y)
            else (insertWhenFct grids simplifiedLine y offset, offset)
      )
    
    insertWhenFct :: [IDT.PositionedGrid] -> Map.Map Int Char -> Int -> Int -> [IDT.PositionedGrid]
    insertWhenFct x line y  offset 
      | List.null x || line == Map.empty = x
      | otherwise = List.init x ++[(Map.insert (y-offset) line (fst (last x)), snd (last x))]
 
findLastChar :: TextAreaContent -> Coord -> IO Coord
findLastChar tac y = do
  let (ChMap hMap _) = charMap tac
  hmap <- readIORef hMap
  let mayLine = Map.lookup y hmap
  return $ if isNothing mayLine then -1 else fst $ List.last $ assocs $ fromJust mayLine
  
-- | returns the size of a TextAreaContent.
size :: TextAreaContent
  -> IO Position -- ^ size of the TextAreaContent
size areaContent = do 
  let (ChMap _ size) = charMap areaContent
  readIORef size

getSelectedPositons :: TextAreaContent -> IO [Position]
getSelectedPositons areaContent = do
  (xMax,yMax) <- TextAreaContent.size areaContent
  filterM (isSelected areaContent) [ (x,y) | y <- [0..yMax], x <- [0..xMax]]
  
isSelected :: TextAreaContent -> Position -> IO Bool
isSelected tac pos = do
  cell <- getCell tac pos
  return $ isJust cell && snd (fst $ fromJust cell)
  
getPositons :: TextAreaContent -> IO [Position]
getPositons areaContent = do
  (xMax,yMax) <- TextAreaContent.size areaContent
  filterM (isOccupied areaContent) [ (x,y) | y <- [0..yMax], x <- [0..xMax]]

getPositonsFrom :: TextAreaContent -> Position -> IO [Position]
getPositonsFrom areaContent (_,y) = do
  (xMax,yMax) <- TextAreaContent.size areaContent
  filterM (isOccupied areaContent) [ (x1,y1) | y1 <- [0..yMax], x1 <- [0..xMax], y1 > y]

-- Is a char at pos ?
isOccupied :: TextAreaContent -> Position -> IO Bool
isOccupied tac pos = do
  cell <- getCell tac pos
  return $ isJust cell

setClipboard :: TextAreaContent -> IO ()
setClipboard tac = do
  positions <- getSelectedPositons tac
  cells <- getCellsByPositions tac positions
  writeIORef (clipboard tac) (List.zip positions cells)

getClipboard :: TextAreaContent -> IO [(Position,(Char,Bool))]
getClipboard tac = readIORef (clipboard tac)

getCellsByPositions :: TextAreaContent -> [Position] -> IO [(Char,Bool)]
getCellsByPositions _ [] = return []
getCellsByPositions tac positions = _getCellsByPositions tac positions []

_getCellsByPositions :: TextAreaContent -> [Position] -> [(Char,Bool)] -> IO [(Char,Bool)]
_getCellsByPositions _ [] cells = return cells
_getCellsByPositions tac (x:xs) cells = do
  cell <- getCell tac x
  _getCellsByPositions tac xs $
    if isJust cell 
    then cells ++ [fst $ fromJust cell]
    else cells
