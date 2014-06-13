module TextArea where

import Preprocessor as Pre
import Lexer
import InterfaceDT as IDT
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

getGrid2dFromPreProc2Lexer(IDT.IPL grid2D) = grid2D

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
    writeIORef size (x,y)
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
    layoutPut layout entry (x*12) (18*y+2)
    hamp <- readIORef hMap
    let hMapN = Map.insert (x,y) entry hamp
    writeIORef hMap hMapN
    on entry keyPressEvent $ do -- TODO: handle Shift_L/Shift_R + dollar,ISO_Level3_Shift + backslash .....
        key <- eventKeyName
        val <- eventKeyVal
        liftIO $ do
            if keyToChar val /= Nothing
            then do
                set entry [entryText := (if keyToChar val == Nothing then "" else [fromJust $ keyToChar val])]
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
            else case key of
                "Return" -> do
                    hmap <- readIORef hMap
                    let nextEntry = Map.lookup (0,y+1) hmap
                    if not $ isJust nextEntry
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
                "Left" -> do
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
                        else do return False
                "Right" -> do
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
                        else do return False
                "Tab" -> do
                    hmap <- readIORef hMap
                    let nextEntry = Map.lookup (x+4,y) hmap
                    if not $ isJust nextEntry
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
                "BackSpace" -> do
                    hmap <- readIORef hMap
                    let prevEntry = Map.lookup (x-1,y) hmap
                    thisChar <- entryGetText entry
                    if thisChar /= ""
                    then do
                        set entry [entryText := ""]
                        return False
                    else do
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
                            else do return False
                otherwise -> do return False
            code <- serializeIt area (0,0) ""
            grid2D <- return $ getGrid2dFromPreProc2Lexer $ Pre.process  (IIP code)
            paintItRed area 0 0
            print "new lexer-performance:"
            highlight area grid2D start
            return True
    return ()

expandXTextAreaN area oldX oldY n
    | n == 0 = do return ()
    | otherwise = do
        expandXTextArea area oldX oldY
        expandXTextAreaN area (succ oldX) oldY (n-1)

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
    expandYTextAreaH area oldX oldY
    (xmax,ymax) <- readIORef size
    writeIORef size (xmax,(succ ymax))

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

-- colors all entry foreground red
paintItRed :: TextArea -> Int -> Int -> IO()
paintItRed textArea x y = do
  (xMax,yMax) <- readIORef $ getPointerToSize textArea
  map <- readIORef $ getPointerToEntryMap textArea
  if x == xMax && y == yMax
  then do
    widgetModifyText (fromJust $ Map.lookup (x,y) map) StateNormal (Color 65535 0 0)
    return ()
  else do
    if x == xMax
    then do
      widgetModifyText (fromJust $ Map.lookup (x,y) map) StateNormal (Color 65535 0 0)
      paintItRed textArea 0 (y+1)
    else do
      widgetModifyText (fromJust $ Map.lookup (x,y) map) StateNormal (Color 65535 0 0)
      paintItRed textArea (x+1) (y)
  return ()
{- to do different colors
 main highlighting function
 Colors:
   comments : red
   $ : orange
   rails : black
   built in function blue
   constans green
-}
highlight :: TextArea -> [Grid2D] -> IP -> IO(IP)
highlight _ [] _ = return crash
highlight textArea grid2D ip = do
  print (show(posx ip)++","++show(posy ip))
  case ip == crash of
   True -> return ip
   _ -> do
    (lex, _)<- return $ parse (head grid2D) ip
    case lex of
      Just NOP -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Boom -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just EOF -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Input -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Output -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Underflow -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just RType -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just (Constant _ )-> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 3372 62381 5732)
      Just (Push _ )-> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just (Pop _) -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just (Call _) -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Add -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Divide -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Multiply -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Substract -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Remainder -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)   
      Just Cut -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Append -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Size -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Nil -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Cons -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Breakup -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Greater -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Equal -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 2478 13810 63262)
      Just Start -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65535 30430 0)
      Just Finish -> do
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65535 30430 0)
      Just (Junction _) -> do -- TODO junction stepping
        changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 65535 30430 0)
        (falseIP,trueIP) <- return $ junctionturns (head grid2D) ip
        highlight textArea grid2D falseIP
        highlight textArea grid2D trueIP
        return ()
      Nothing -> changeColorOfEntryByCoord textArea (posx ip,posy ip) (Color 0 0 0)
    if lex == (Just (Junction 0))-- after a junction dont highlight again!
    then return crash
    else do
      nexIP <- return $ step (head grid2D) ip
      highlight textArea grid2D nexIP 
-- Serializes the code and delets whitespaces at the end of lines
serializeIt :: TextArea 
    -> (Int,Int)
    -> String
    -> IO(String)
serializeIt textArea (w,h) code = do
  (x,y) <-  readIORef $ getPointerToSize textArea
  case h > y of
    True -> do return code
    _ -> do
      map <- readIORef $ getPointerToEntryMap textArea
      line <- serializeItHelp map (w,h) (x,y) ""
      clearLine <- return $ (reverse.dropWhile(==' ').reverse) line
      serializeIt textArea (0,h+1) (code++(line++"\n"))

serializeItHelp :: Map (Int,Int) Entry
    -> (Int,Int)
    -> (Int,Int)
    -> String
    -> IO(String)
serializeItHelp map (w,h) (xMax,yMax) line = do
  case w >= xMax of
    True -> return (line)
    _ -> do
      elem <- return $ Map.lookup (w,h) map
      case isNothing(elem) of
        True -> serializeItHelp map (w+1,h) (xMax,yMax) (line++" ")
        _ -> do
          entry <- return $ fromJust elem
          content <- entryGetText entry
          serializeItHelp map (w+1,h) (xMax,yMax) (line++content)

serializeTextAreaContent area@(TextArea layout current hMap size) = do
    hmap <- readIORef hMap
    let list = toList hmap
    let sortedList = quicksort list
    result <- listToString sortedList [] 0
    let rightOrder = unlines $ Prelude.filter (/="") $ Prelude.map (\x -> reverse $ dropWhile (==' ') $ reverse x) (lines $ reverse result)
    return rightOrder
        where
        quicksort :: [((Int,Int),Entry)] -> [((Int,Int),Entry)]
        quicksort [] = []
        quicksort (x:xs) = quicksort [a | a <- xs, before a x] ++ [x] ++ quicksort [a | a <- xs, not $ before a x]

        listToString :: [((Int,Int),Entry)] -> String -> Int -> IO String
        listToString list akku beforeY = do
            if (length list) == 0
            then return akku
            else do
                text <- entryGetText (snd $ head list)
                let (x,y) = fst $ head list
                if y > beforeY
                then listToString (tail list) ((head text) : '\n' : akku) y
                else listToString (tail list) ((head text) : akku) y

        before :: ((Int,Int),Entry) -> ((Int,Int),Entry) -> Bool
        before ((a,b),_) ((c,d),_) = b < d || (b == d && a <= c)
