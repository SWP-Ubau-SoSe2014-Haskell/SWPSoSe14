module TextArea where

import Lexer
import InterfaceDT
import Graphics.UI.Gtk
import Data.Map as Map
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.Maybe

data EntryMode = LeftToRight | UpToDown | Smart deriving (Eq)
data TextArea = TextArea Layout (IORef (Int,Int)) (IORef (Map.Map (Int,Int) Entry)) (IORef (Int,Int))

getLayout (TextArea layout _ _ _) = layout

getPointerToCurrentInFocus (TextArea _ current _ _) = current

getPointerToEntryMap (TextArea _ _ map _) = map

getPointerToSize (TextArea _ _ _ size) = size


textAreaNew :: Layout -> Int -> Int -> IO(TextArea)
textAreaNew layout x y = do
    currentInFocus <- newIORef (0,0)
    hashMap <- newIORef Map.empty
    size <- newIORef (0,0)
    let area =  TextArea layout currentInFocus hashMap size
    createTextArea area x y
    return area

createTextArea :: TextArea -> Int -> Int -> IO()
createTextArea area@(TextArea layout current hmap size) x y = do
    createTextAreaH area 0 (pred x) 0 (pred y)
    writeIORef size (x-1,y-1)
    return ()

createTextAreaH :: TextArea -> Int -> Int -> Int -> Int -> IO()
createTextAreaH area@(TextArea layout current hamp size) xnr xnrS ynr ynrS = do
    (maxX,maxY) <- readIORef size
    if xnr == xnrS && ynr == ynrS
    then do
        entryInsert area xnrS xnrS
    else if xnr == xnrS && ynr < ynrS
        then do
            entryInsert area  xnr ynr
            createTextAreaH area 0 xnrS (succ ynr) ynrS
        else do
            entryInsert area xnr ynr
            createTextAreaH area (succ xnr) xnrS ynr ynrS

entryInsert :: TextArea -> Int -> Int -> IO()
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
    on entry keyPressEvent $ do -- TODO: handle Shift_L/Shift_R + dollar,ISO_Level3_Shift + backslash .....
        key <- eventKeyName 
        liftIO $ if isSimpleChar key
            then do
                grid2D <- buildGrid2d area (0,0) []
                highlight area grid2D start
                set entry [entryText := key]
                hmap <- readIORef hMap
                let nextEntry = Map.lookup (x+1,y) hmap
                if not $ isJust nextEntry
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
            else do
                grid2D <- buildGrid2d area (0,0) []
                highlight area grid2D start 
                return False
    return ()
            where isSimpleChar x = elem x $ Prelude.map (\x -> [x])  (['a'..'z']++['A'..'Z']++" $\\/|-+x*")


expandXTextArea area@(TextArea layout current hMap size) oldX oldY= do
    expandXTextAreaH area oldX oldY
    (xmax,ymax) <- readIORef size
    writeIORef size ((succ xmax),ymax)

expandXTextAreaH area@(TextArea _ _ hMap _) oldX oldY = do
    if oldY == 0
    then do
        entryInsert area (succ oldX) 0
        hmap <- readIORef hMap
        let newEntry = fromJust $ Map.lookup ((succ oldX),oldY) hmap
        widgetShow newEntry
    else do
        entryInsert area (succ oldX) oldY
        hmap <- readIORef hMap
        let newEntry = fromJust $ Map.lookup ((succ oldX),oldY) hmap
        widgetShow newEntry
        expandXTextAreaH area oldX (pred oldY)


expandYTextArea area@(TextArea layout current hMap size) oldX oldY= do
    expandXTextAreaH area oldX oldY
    (xmax,ymax) <- readIORef size
    writeIORef size ((succ xmax),ymax)

expandYTextAreaH area@(TextArea _ _ hMap _) oldX oldY = do
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

clearEntryByCoord :: TextArea -> (Int,Int) -> IO()
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

-- to do different colors
-- main highlighting function
highlight :: TextArea -> Grid2D -> IP -> IO(IP)
highlight textArea grid2D ip = do
  case ip == crash of
   True -> return ip
   _ -> do
    (lex, _)<- return $ parse grid2D ip
    case lex of
      Just NOP -> do
            changeColorOfEntryByCoord textArea (posx ip,posy ip) ((Color 65000 0 0))
      Just Boom -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just EOF -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Input -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Output -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Underflow -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just RType -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just (Constant _ )-> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just (Push _ )-> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just (Pop _) -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just (Call _) -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Add -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Divide -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Multiply -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Remainder -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Substract -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Cut -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Append -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Size -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Nil -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Cons -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Breakup -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Greater -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Equal -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Start -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just Finish -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Just (Junction _) -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65000 0 0)
      Nothing -> changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 0 0 0)
    nexIP <- return $ step grid2D ip
    print ip
    highlight textArea grid2D nexIP
-- builds a grid2D needed by Lexer
buildGrid2d :: TextArea 
    -> (Int,Int)
    -> Grid2D
    -> IO(Grid2D)
buildGrid2d textArea (w,h) grid2D = do
  (x,y) <-  readIORef $ getPointerToSize textArea
  case h > y of
    True -> do return grid2D
    _ -> do
      map <- readIORef $ getPointerToEntryMap textArea
      grid <- buildHelp map (w,h) (x,y) grid2D
      buildGrid2d textArea (0,h+1) grid

buildHelp :: Map (Int,Int) Entry
    -> (Int,Int)
    -> (Int,Int)
    -> Grid2D
    -> IO(Grid2D)
buildHelp map (w,h) (xMax,yMax) grid2D = do
  case w >= xMax of
    True -> return (grid2D)
    _ -> do
      (xs,x) <- return $ splitAt (length grid2D) grid2D
      elem <- return $ Map.lookup (w,h) map
      case isNothing(elem) of
        True -> do
          if x == [] 
          then buildHelp map (w+1,h) (xMax,yMax) (xs++[" "])
          else buildHelp map (w+1,h) (xMax,yMax) (xs++[(head x)++" "])
        _ -> do
          entry <- return $ fromJust elem
          content <- entryGetText entry
          print content
          if x == []
          then buildHelp map (w+1,h) (xMax,yMax) (xs++[content])
          else buildHelp map (w+1,h) (xMax,yMax) (xs++[(head x)++content])
