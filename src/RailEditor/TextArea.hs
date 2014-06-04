module TextArea where

import Graphics.UI.Gtk
import Data.Map as Map
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.Maybe

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
    set entry [entryWidthChars := 1]
    entrySetMaxLength entry 1
    entrySetHasFrame entry False
    entry `on` focusInEvent $ tryEvent $ liftIO $ writeIORef current (x,y)
    layoutPut layout entry (x*10) (18*y+24)
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
                    let nextEntry = Map.lookup (0,y+1) hmap
                    if not $ isJust nextEntry
                    then return False
                    else do
                        let nEntry = fromJust nextEntry
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
