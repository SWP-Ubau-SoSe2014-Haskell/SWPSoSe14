module TextArea where

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
