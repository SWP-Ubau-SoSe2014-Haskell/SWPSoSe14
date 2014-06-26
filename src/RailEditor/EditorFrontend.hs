module EditorFrontend where 

import Graphics.Rendering.Cairo hiding (width, height, Content)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events as Events
import Data.IORef
import Data.Maybe
import Data.Map as Map
import Control.Monad
import Graphics.UI.Gtk.Gdk.GC hiding (fill)
import Graphics.UI.Gtk hiding (Color)
import EditorTypes
import EditorContent

main :: IO ()
main = do
  initGUI
  vAdjustment <- adjustmentNew 0 0 0 hef 0 0
  hAdjustment <- adjustmentNew 0 0 0 bef 0 0
  scrwin <- scrolledWindowNew (Just hAdjustment) (Just vAdjustment)
  drawingArea <- setUpDrawingArea
  scrolledWindowAddWithViewport scrwin drawingArea
  posRef <- newIORef (0.0,0.0)
  contentRef <- newIORef Map.empty
  setUpWindow scrwin
                                                                  
  onButtonPress drawingArea $ \event -> do
    readIORef posRef >>= clearCursor drawingArea
    handleButtonPress drawingArea (eventX event,eventY event) >>= writeIORef posRef
    return $ Events.eventSent event

  onKeyPress drawingArea $ \event -> do
    readIORef posRef >>= clearCursor drawingArea
    pos@(x,y) <- readIORef posRef
    let key = Events.eventKeyName event
    captureArrowKeys drawingArea pos hAdjustment vAdjustment key >>= writeIORef posRef
    captureNonPrintableKeys drawingArea contentRef posRef (x,y) vAdjustment hAdjustment key
    char <- getCharacter event
    when (isJust char) $ displayCharacter drawingArea contentRef (ContentEntry pos $ fromJust char) hAdjustment >>= writeIORef posRef
    readIORef posRef >>= showCursor drawingArea 												
    return $ Events.eventSent event  

  onExpose drawingArea $ \(Expose sent _ _ _) -> do
    content <- readIORef contentRef
    readIORef posRef >>= redrawDrawingArea drawingArea (Map.toList content) 
    return sent        
  --timeoutAddFull (widgetQueueDraw drawingArea >> return True) priorityDefaultIdle 100    
  mainGUI

captureNonPrintableKeys :: DrawingArea -> Content -> CurrentPosition -> Position -> Adjustment -> Adjustment -> String -> IO ()
captureNonPrintableKeys drawingArea contentRef posRef (x,y) vAdjustment hAdjustment key = do
  case key of 
    "Return" ->  do
      handleReturn drawingArea contentRef (x,y) vAdjustment
      writeIORef posRef (x,y + hef)
    "BackSpace" -> handleBackSpace drawingArea contentRef (x,y) hAdjustment >>= writeIORef posRef
    _ -> return ()

setUpWindow :: ScrolledWindow -> IO ()
setUpWindow scrwin = do
  window <- windowNew
  window `onDestroy` mainQuit
  set window [windowDefaultWidth := (round width), windowDefaultHeight := (round height)]
  window `containerAdd` scrwin  
  widgetShowAll window
 
setUpDrawingArea :: IO (DrawingArea)
setUpDrawingArea = do
  drawingArea <- drawingAreaNew
  widgetModifyBg drawingArea StateNormal defaultBgColor
  set drawingArea [widgetCanFocus := True]
  widgetSetSizeRequest drawingArea (round width) (round height)
  return drawingArea
  
handleReturn :: DrawingArea -> Content -> Position -> Adjustment -> IO ()
handleReturn drawingArea contentRef (x,y) vAdjustment = do
  content <- captureReturn contentRef (x,y)
  redrawCharacters drawingArea content 0 hef
  let newY = if Prelude.null content then y else Prelude.foldl (\y1 ((_,y2),_) -> max y1 y2) 0.0 content
  extendDrawingAreaVertically drawingArea vAdjustment newY

displayCharacter :: DrawingArea -> Content -> ContentEntry -> Adjustment -> IO (Position)
displayCharacter drawingArea contentRef entry@(ContentEntry (x,y) char) adjustment = do 			
  addCharacter contentRef entry
  renderScene drawingArea entry black								-- GUI
  extendDrawingAreaHorizontally drawingArea adjustment x			-- GUI 	
  return (x+bef,y)

handleBackSpace :: DrawingArea -> Content -> Position -> Adjustment -> IO (Position)
handleBackSpace drawingArea contentRef (x,y) adjustment
  | (x == 0) && (y >= hef) = do
    (map1,pos,map2) <- backSpaceLine y contentRef adjustment
    redrawCharactersByCoordinates drawingArea pos map1					-- GUI
    redrawCharacters drawingArea map2 0 (-hef)							-- GUI
    return pos
  | (y >= 0) && (x >= bef) = do
    newX <- updateDirection x bef adjustment
    map1 <- shiftLeftLine contentRef (newX,y) adjustment
    removeCharacter drawingArea (newX,y)								-- GUI
    redrawCharacters drawingArea map1 (-bef) 0							-- GUI
    return (newX,y)
  | otherwise = return (x,y)

getCharacter :: Event -> IO (Maybe Char)
getCharacter event = 
  let val = Events.eventKeyVal event
  in return $ keyToChar val
  
captureArrowKeys :: DrawingArea -> Position -> Adjustment -> Adjustment -> String -> IO (Position)
captureArrowKeys drawingArea (x,y) hAdjustment vAdjustment key = do
  case key of
    "Left" 	-> do
      newX <- updateDirection x bef hAdjustment
      return (newX,y)
    "Right" -> do 
      extendDrawingAreaHorizontally drawingArea hAdjustment x
      return (x+bef,y) 
    "Up" 	-> do 
      newY <- updateDirection y hef vAdjustment
      return (x,newY)
    "Down" 	-> do
      extendDrawingAreaVertically drawingArea vAdjustment y      
      return (x,y + hef)
    _ -> return (x,y) 

updateDirection :: Double -> Double -> Adjustment -> IO (Double)
updateDirection direction size adjustment = do
  if (direction >= size) then do
    value <- adjustmentGetValue adjustment
    when (direction - size < value) $ do adjustmentSetValue adjustment $ direction - size
    return $ direction - size
  else return direction

handleButtonPress :: DrawingArea -> Position -> IO (Position)  
handleButtonPress drawingArea pos = do
  newPos <- updatePosition pos 
  showCursor drawingArea newPos
  return newPos  

renderScene :: DrawingArea -> ContentEntry -> RGBColor -> IO ()
renderScene drawingArea (ContentEntry (x,y) char) (RGBColor r g b) = do
  removeCharacter drawingArea (x,y)
  drawWindow <- widgetGetDrawWindow drawingArea
  renderWithDrawable drawWindow $ do 
    setSourceRGBA r g b 1.0
    moveTo (x+2) (y+hef-3)
    setFontSize 14
    showText [char]

removeCharacter :: DrawingArea -> Position -> IO ()     
removeCharacter drawingArea (x,y) = do
  drawWindow <- widgetGetDrawWindow drawingArea
  drawWindowClearArea drawWindow (round x) (round y) (round bef) (round hef) 

extendDrawingAreaHorizontally :: DrawingArea -> Adjustment -> Double -> IO ()
extendDrawingAreaHorizontally drawingArea adjustment x = do 
  (w,h) <- widgetGetSizeRequest drawingArea
  value <- adjustmentGetValue adjustment
  when (width + value - x - bef < bef) $ do
    when (round (x + bef) >= w) $ widgetSetSizeRequest drawingArea (w + (round bef)) h
    adjustmentSetValue adjustment $ value + bef

extendDrawingAreaVertically :: DrawingArea -> Adjustment -> Double -> IO ()
extendDrawingAreaVertically drawingArea adjustment y = do 
  (w,h) <- widgetGetSizeRequest drawingArea
  value <- adjustmentGetValue adjustment
  when (height + value - y - hef < hef) $ do
    when (round (y + hef) >= h) $ widgetSetSizeRequest drawingArea w (h + (round hef))
    adjustmentSetValue adjustment $ value + hef

redrawCharacters :: DrawingArea -> ContentList -> Double -> Double -> IO ()
redrawCharacters _ [] _ _ = return ()
redrawCharacters drawingArea ((pos@(x,y),char):xs) bef hef = do
  redrawCharacters drawingArea xs bef hef
  removeCharacter drawingArea (x,y)								
  renderScene drawingArea (ContentEntry (x+bef,y+hef) char) black	
  
redrawCharactersByCoordinates :: DrawingArea -> Position -> ContentList -> IO ()  
redrawCharactersByCoordinates _ _ [] = return ()  
redrawCharactersByCoordinates drawingArea (x1,y1) (((x2,y2),char):xs) = do
  removeCharacter drawingArea (x2,y2)
  renderScene drawingArea (ContentEntry (x2+x1,y1) char) black
  redrawCharactersByCoordinates drawingArea (x1,y1) xs

redrawDrawingArea :: DrawingArea -> ContentList -> Position -> IO ()
redrawDrawingArea drawingArea content pos = do
  showCursor drawingArea pos
  redrawContent drawingArea content

redrawContent :: DrawingArea -> ContentList -> IO ()  
redrawContent _ [] = return ()
redrawContent drawingArea (x@(pos,char):xs) = do
 renderScene drawingArea (ContentEntry pos char) black
 redrawContent drawingArea xs

showCursor :: DrawingArea -> Position -> IO ()
showCursor drawingArea (x,y) = do
  drawWindow <- widgetGetDrawWindow drawingArea
  gc <- gcNew drawWindow
  gcSetValues gc $ newGCValues { foreground = defaultCursorColor }
  drawRectangle drawWindow gc True (round x) (round y) 2 (round hef)
  return ()

clearCursor :: DrawingArea -> Position -> IO ()
clearCursor drawingArea (x,y) = do
  drawWindow <- widgetGetDrawWindow drawingArea
  drawWindowClearArea drawWindow (round x) (round y) 2 (round hef) 
