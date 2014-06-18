module TextArea where

import Preprocessor as Pre
import Lexer
import InterfaceDT as IDT
import Graphics.UI.Gtk
import Data.Map as Map
import Control.Monad.Trans (liftIO)
import qualified Control.Exception as Exc
import System.IO
import Data.IORef
import Data.Maybe
import Data.Either

data EntryMode = LeftToRight | UpToDown | Smart deriving (Eq)
data TextArea = TextArea 
  Layout (IORef (Int,Int))
  (IORef (Map.Map (Int,Int) Entry))
  (IORef (Int,Int))

getLayout (TextArea layout _ _ _) = layout

getPointerToCurrentInFocus (TextArea _ current _ _) = current

getPointerToEntryMap (TextArea _ _ map _) = map

getPointerToSize (TextArea _ _ _ size) = size

getGrid2dFromPreProc2Lexer(IDT.IPL grid2D) = grid2D

textAreaNew :: Layout
  -> Int
  -> Int
  -> IO TextArea
textAreaNew layout x y = do
  currentInFocus <- newIORef (0,0)
  hashMap <- newIORef Map.empty
  size <- newIORef (0,0)
  let area =  TextArea layout currentInFocus hashMap size
  createTextArea area x y
  return area

createTextArea :: TextArea
  -> Int
  -> Int
  -> IO()
createTextArea area@(TextArea layout current hmap size) x y = do
  createTextAreaH area 0 (pred x) 0 (pred y)
  writeIORef size (x-1,y-1)
  return ()

createTextAreaH :: TextArea
  -> Int
  -> Int
  -> Int
  -> Int
  -> IO()
createTextAreaH area@(TextArea _ _ _ size) xnr xnrS ynr ynrS = do
  (maxX,maxY) <- readIORef size
  if xnr == xnrS && ynr == ynrS
  then entryInsert area xnrS xnrS
  else if xnr == xnrS && ynr < ynrS
  then do
    entryInsert area  xnr ynr
    createTextAreaH area 0 xnrS (succ ynr) ynrS
  else do
    entryInsert area xnr ynr
    createTextAreaH area (succ xnr) xnrS ynr ynrS


handleReturn area@(TextArea layout current hMap size)x y = do
  hmap <- readIORef hMap
  let nextEntry = Map.lookup (0,y+1) hmap
  if isNothing nextEntry
  then do
    (xm,ym) <- readIORef size
    expandYTextArea area xm ym
    hmap <- readIORef hMap
    let nEntry = fromJust $ Map.lookup (0,y+1) hmap
    widgetGrabFocus nEntry
    return True
  else do
    let nEntry = fromJust nextEntry
    widgetGrabFocus nEntry
    return True

handleLeft area@(TextArea layout current hMap size)x y = do
  hmap <- readIORef hMap
  let prevEntry = Map.lookup (x-1,y) hmap
  if isJust prevEntry
  then do
    widgetGrabFocus (fromJust prevEntry)
    return True
  else do
    (xm,ym) <- readIORef size
    if y>0
    then do
      widgetGrabFocus $ fromJust $ Map.lookup (xm, y-1) hmap
      return True
    else return False

handleRight area@(TextArea layout current hMap size)x y = do
  hmap <- readIORef hMap
  let nextEntry = Map.lookup (x+1,y) hmap
  if isJust nextEntry
  then do
    widgetGrabFocus $ fromJust nextEntry
    return True
  else do
    (xm,ym) <- readIORef size
    if y<ym
    then do
      widgetGrabFocus $ fromJust $ Map.lookup (0, y+1) hmap
      return True
    else return False

handleUp area@(TextArea layout current hMap size) x y = do
  hmap <- readIORef hMap
  let nextEntry = Map.lookup (x,y-1) hmap
  if isJust nextEntry
  then do
    widgetGrabFocus $ fromJust nextEntry
    return True
  else return False

handleDown area@(TextArea layout current hMap size) x y = do
  hmap <- readIORef hMap
  let nextEntry = Map.lookup (x,y+1) hmap
  if isJust nextEntry
  then do
    widgetGrabFocus $ fromJust nextEntry
    return True
  else return False

handleTab area@(TextArea layout current hMap size)x y = do
  hmap <- readIORef hMap
  let nextEntry = Map.lookup (x+4,y) hmap
  if isNothing nextEntry
  then do
    (xm,ym) <- readIORef size
    expandXTextAreaN area xm ym 4
    hmap <- readIORef hMap
    let nEntry = fromJust $ Map.lookup (x+4,y) hmap
    widgetGrabFocus nEntry
    return True
  else do
    let nEntry = fromJust nextEntry
    widgetGrabFocus nEntry
    return True
    
handleBackspace area@(TextArea layout current hMap size) entry x y = do
  hmap <- readIORef hMap
  let prevEntry = Map.lookup (x-1,y) hmap
  thisChar <- entryGetText entry
  if thisChar /= ""
  then do
    set entry [entryText := ""]
    return False
  else
    if isJust prevEntry
    then do
      entrySetText (fromJust prevEntry) ""
      widgetGrabFocus (fromJust prevEntry)
      return True
    else do
      (xm,ym) <- readIORef size
      if y>0
      then do
        widgetGrabFocus $ fromJust $ Map.lookup (xm, y-1) hmap
        return True
      else return False

entryInsert :: TextArea
  -> Int
  -> Int
  -> IO()
entryInsert area@(TextArea layout current hMap size) x y = do
  entry <- entryNew
  set entry [entryWidthChars := 1, entryText := " "]
  entrySetMaxLength entry 1
  entrySetHasFrame entry False
  entry `on` focusInEvent $ tryEvent $ liftIO $ writeIORef current (x,y)
  layoutPut layout entry (x*12) (18*y+20)
  hamp <- readIORef hMap
  let hMapN = Map.insert (x,y) entry hamp
  writeIORef hMap hMapN
  on entry keyPressEvent $ do 
    key <- eventKeyName
    val <- eventKeyVal
    liftIO $ do
      if isJust (keyToChar val)
      then do
        set entry [entryText := (
          if isNothing (keyToChar val)
          then "" 
          else [fromJust $ keyToChar val])]
        hmap <- readIORef hMap
        let nextEntry = Map.lookup (x+1,y) hmap
        if isNothing nextEntry
        then do
          (xm,ym) <- readIORef size
          expandXTextArea area xm ym
          hmap <- readIORef hMap
          let nEntry = fromJust $ Map.lookup (x+1,y) hmap
          widgetGrabFocus nEntry
          return True
        else do
          let nEntry = fromJust nextEntry
          widgetGrabFocus nEntry
          return True
        else case key of
          "Return" -> handleReturn area x y
          "Left" -> handleLeft area x y
          "Right" -> handleRight area x y
          "Tab" -> handleTab area x y
          "BackSpace" -> handleBackspace area entry x y
          "Up" -> handleUp area x y
          "Down" -> handleDown area x y
          _ -> return False
      (code,indexes) <- serializeIt area (0,0) ("",[])
      Exc.catch (do
        let grid2D = getGrid2dFromPreProc2Lexer $ Pre.process  (IIP code)
        (xm,ym) <- readIORef size
        paintItRed area x y xm ym
        changeColorOfCurrentEntry area (Color 65535 0 0)
        highlightFcts area grid2D indexes 
        return ()) handler
      return True    
  return ()

handler :: Exc.ErrorCall -> IO ()
handler _ = putStrLn "No main function"

expandXTextAreaN area oldX oldY n
  | n == 0 = return ()
  | otherwise = do
    expandXTextArea area oldX oldY
    expandXTextAreaN area (succ oldX) oldY (n-1)

expandXTextArea area@(TextArea layout current hMap size) oldX oldY= do
  expandXTextAreaH area oldX oldY
  (xmax,ymax) <- readIORef size
  writeIORef size (succ xmax,ymax)

expandXTextAreaH area@(TextArea _ _ hMap _) oldX oldY = 
  if oldY == 0
  then do
    entryInsert area (succ oldX) 0
    hmap <- readIORef hMap
    let newEntry = fromJust $ Map.lookup (succ oldX,oldY) hmap
    widgetShow newEntry
  else do
    entryInsert area (succ oldX) oldY
    hmap <- readIORef hMap
    let newEntry = fromJust $ Map.lookup (succ oldX,oldY) hmap
    widgetShow newEntry
    expandXTextAreaH area oldX (pred oldY)


expandYTextArea area@(TextArea layout current hMap size) oldX oldY= do
  expandYTextAreaH area oldX oldY
  (xmax,ymax) <- readIORef size
  writeIORef size (xmax,succ ymax)

expandYTextAreaH area@(TextArea _ _ hMap _) oldX oldY = 
  if oldX == 0
  then do
    entryInsert area 0 (succ oldY)
    hmap <- readIORef hMap
    let newEntry = fromJust $ Map.lookup (oldX,succ oldY) hmap
    widgetShow newEntry
  else do
    entryInsert area oldX (succ oldY)
    hmap <- readIORef hMap
    let newEntry = fromJust $ Map.lookup (oldX,succ oldY) hmap
    widgetShow newEntry
    expandYTextAreaH area (pred oldX) oldY

clearEntryByCoord :: TextArea
  -> (Int,Int)
  -> IO()
clearEntryByCoord (TextArea _ _ hMap _) (x,y) = do
  hashMap <- readIORef hMap
  let mayEntry = Map.lookup (x,y) hashMap
  if isJust mayEntry
  then do
    let entry = fromJust mayEntry
    set entry [entryText := ""]
  else return ()

clearCurrentEntry :: TextArea -> IO()
clearCurrentEntry (TextArea _ current hMap _) = do
  currentCoord <- readIORef current
  hashMap <- readIORef hMap
  let currentEntry = fromJust $ Map.lookup currentCoord hashMap
  set currentEntry [entryText := ""]

changeColorOfEntryByCoord :: TextArea -> (Int,Int) -> Color -> IO()
changeColorOfEntryByCoord (TextArea _ _ hMap _) (x,y) color = do
  hashMap <- readIORef hMap
  let mayEntry = Map.lookup (x,y) hashMap
  if isJust mayEntry
  then do
    let entry = fromJust mayEntry
    widgetModifyText entry StateNormal color
  else return ()

changeColorOfCurrentEntry :: TextArea -> Color -> IO()
changeColorOfCurrentEntry (TextArea _ current hMap _) color = do
  currentCoord <- readIORef current
  hashMap <- readIORef hMap
  let currentEntry = fromJust $ Map.lookup currentCoord hashMap
  widgetModifyText currentEntry StateNormal color

-- colors all entry red in a rect from x,y to xMax,yMax
-- This function is needed to recolor after editing
paintItRed :: TextArea 
  -> Int
  -> Int
  -> Int
  -> Int
  -> IO()
paintItRed textArea x y xMax yMax= do
  map <- readIORef $ getPointerToEntryMap textArea
  let entry = Map.lookup (x,y) map
  case entry of
    Nothing -> return ()
    _ ->
      if x == (xMax-1) && y == (yMax-1)
      then do
        widgetModifyText (fromJust entry) StateNormal red
        return ()
      else
        if x == (xMax-1)
        then do
          widgetModifyText (fromJust entry) StateNormal red
          paintItRed textArea 0 (y+1) xMax yMax
        else do
          widgetModifyText (fromJust entry) StateNormal red
          paintItRed textArea (x+1) y xMax yMax
  return ()
  where red = Color 65535 0 0
  
-- highlight all rail-functions
highlightFcts :: TextArea
  -> [Grid2D]  
  -> [Int]
  -> IO IP
highlightFcts area [] _ = return crash
highlightFcts area _ [] = return crash
highlightFcts area (x:xs) (y:ys) = do
  highlight area x start y
  highlightFcts area xs ys
  
{- to do different colors
 main highlighting process which highlights a single rail-function.
 Colors:
   comments : red
   $ : orange
   rails : black
   built in function blue
   constans green
-}
highlight :: TextArea
  -> Grid2D
  -> IP
  -> Int
  -> IO IP
highlight _ [] _ _ = return crash
highlight textArea grid2D ip yOffset = do
  print ip
  case ip == crash of
   True -> return ip
   _ -> do
    (lex, parseIP)<- return $ parse grid2D ip
    case lex of
      Just NOP              -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Boom             -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just EOF              -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Input            -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Output           -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just IDT.Underflow    -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just RType            -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just (Constant _ )    -> changeColorOfEntryByCoord textArea (xC,yC) green
      Just (Push str)-> do
        colorMoves textArea grid2D (length str+2)
          (turnaround parseIP) blue
        highlight textArea grid2D (step grid2D parseIP)yOffset
        return ()
      Just (Pop str) -> do
        colorMoves textArea grid2D (length str+4)
          (turnaround parseIP) blue
        highlight textArea grid2D (step grid2D parseIP)yOffset
        return ()
      Just (Call str) -> do
        colorMoves textArea grid2D (length str+2)
          (turnaround parseIP) blue
        highlight textArea grid2D (step grid2D parseIP)yOffset
        return ()
      Just Add1             -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Divide           -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Multiply         -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Subtract         -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Remainder        -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Cut              -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Append           -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Size             -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Nil              -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Cons             -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Breakup          -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Greater          -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Equal            -> changeColorOfEntryByCoord textArea (xC,yC) blue
      Just Start            -> changeColorOfEntryByCoord textArea (xC,yC) gold
      Just Finish           -> changeColorOfEntryByCoord textArea (xC,yC) gold
      Just (Junction _) -> do
        changeColorOfEntryByCoord textArea (xC,yC) gold
        (falseIP,trueIP) <- return $ junctionturns grid2D ip
        highlight textArea grid2D falseIP yOffset
        highlight textArea grid2D trueIP yOffset
        return ()
      Nothing               -> changeColorOfEntryByCoord textArea (xC,yC) black
    case lex of
      Just (Junction 0) -> return crash
      Just (Push _) -> return crash
      Just (Pop _) -> return crash
      Just (Call _) -> return crash
      _ -> do
        let nexIP = step grid2D ip
        highlight textArea grid2D nexIP yOffset
    where
      xC = posx ip
      yC = posy ip+yOffset
      blue = Color 2478 13810 63262
      green = Color 3372 62381 5732
      gold = Color 65535 30430 0
      black = Color 0 0 0
      {- moves the grid and colors the entrys used to handel Push Pop
        and Call
      -}
      colorMoves :: TextArea -> Grid2D -> Int -> IP -> Color -> IO IP
      colorMoves _ _ 0  _ _ = return crash
      colorMoves area grid2D stepsBack ip color = do
        changeColorOfEntryByCoord area (posx ip,posy ip+yOffset) color
        colorMoves area grid2D (stepsBack-1) (move ip Forward) color
        return crash
        


{-Serializes the code and delets whitespaces at the end of lines.
  It also returns the y coord of $ of functions
-}
serializeIt :: TextArea 
  -> (Int,Int)
  -> (String,[Int])
  -> IO(String,[Int])
serializeIt textArea (w,h) (code,indexes) = do
  (x,y) <-  readIORef $ getPointerToSize textArea
  if h > y then return (code, indexes) else
    (do
      map <- readIORef $ getPointerToEntryMap textArea
      line <- serializeItHelp map (w,h) (x,y) ""
      let clearLine = (reverse.dropWhile(==' ').reverse) line
      serializeIt textArea (0, h + 1)
        (if not (Prelude.null clearLine) && head clearLine == '$' then
            (code ++ (line ++ "\n"), indexes ++ [h]) else
            (code ++ (line ++ "\n"), indexes)))

serializeItHelp :: Map (Int,Int) Entry
  -> (Int,Int)
  -> (Int,Int)
  -> String
  -> IO String
serializeItHelp map (w,h) (xMax,yMax) line = 
  if w >= xMax then return line else
    (do
      let elem = Map.lookup (w,h) map
      if isNothing elem then
        serializeItHelp map (w+1,h) (xMax,yMax) (line++" ") else
        (do
          let entry = fromJust elem
          content <- entryGetText entry
          serializeItHelp map (w+1,h) (xMax,yMax) (line++content)))

serializeTextAreaContent area@(TextArea layout current hMap size) = do
  hmap <- readIORef hMap
  let list = toList hmap
  let sortedList = quicksort list
  result <- listToString sortedList [] 0
  let rightOrder = unlines $ Prelude.filter (/="") $ Prelude.map (reverse . dropWhile (== ' ') . reverse) (lines $ reverse result)
  return rightOrder
    where
      quicksort :: [((Int,Int),Entry)] -> [((Int,Int),Entry)]
      quicksort [] = []
      quicksort (x:xs) = quicksort [a | a <- xs, before a x] ++ [x] ++ quicksort [a | a <- xs, not $ before a x]

      listToString :: [((Int,Int),Entry)] -> String -> Int -> IO String
      listToString list akku beforeY = 
        if Prelude.null list
        then return akku
        else do
          text <- entryGetText (snd $ head list)
          let (x,y) = fst $ head list
          if y > beforeY
          then listToString (tail list) (head text : '\n' : akku) y
          else listToString (tail list) (head text : akku) y

      before :: ((Int,Int),Entry) -> ((Int,Int),Entry) -> Bool
      before ((a,b),_) ((c,d),_) = b < d || (b == d && a <= c)
