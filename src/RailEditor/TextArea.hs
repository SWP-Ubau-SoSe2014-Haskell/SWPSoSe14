{- |
Module      :  TextArea.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT
Stability   :  experimental

The TextArea-module depicts the view on the data structure stored in the TextAreaContent-module for the editor.
-}


module TextArea where
    
    -- imports --
import TextAreaContent as TAC
import qualified KeyHandler
import TextAreaContentUtils
import Highlighter as HIGH

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events as Events
import Data.IORef
import Data.Maybe
import Data.Map as Map
import Control.Monad
import Graphics.UI.Gtk.Gdk.GC hiding (fill)
import Graphics.UI.Gtk hiding (Color)


data TextArea = TA { drawingArea :: DrawingArea,
    textAreaContent :: IORef (TextAreaContent),
    scrolledWindow :: ScrolledWindow,
    currentPosition :: IORef (Position),
    vAdjustment :: Adjustment,
    hAdjustment :: Adjustment }

defaultFgColor :: Color
defaultFgColor = Color 0 0 0 

defaultCursorColor :: Color
defaultCursorColor = Color 65535 0 0 

defaultBgColor :: Color
defaultBgColor = Color 65535 65535 65535

defaultRowColumSelectColor :: Color
defaultRowColumSelectColor = Color 60000 60000 60000

width = 600.0
height = 400.0

getTextAreaContainer :: TextArea -> IO (ScrolledWindow)
getTextAreaContainer textArea = return $ scrolledWindow textArea

setTextAreaContent :: TextArea -> TextAreaContent -> IO ()
setTextAreaContent textArea areaContent= do
  let areaRef = textAreaContent textArea
  writeIORef areaRef areaContent

initTextAreaWithContent :: TextAreaContent -> IO (TextArea)
initTextAreaWithContent areaContent = do
  veAdjustment <- adjustmentNew 0 0 0 hef 0 0
  hoAdjustment <- adjustmentNew 0 0 0 bef 0 0
  scrwin <- scrolledWindowNew (Just hoAdjustment) (Just veAdjustment)
  drawArea <- setUpDrawingArea
  scrolledWindowAddWithViewport scrwin drawArea
  posRef <- newIORef (0.0,0.0)
  areaRef <- newIORef areaContent
  let textArea = TA drawArea areaRef scrwin posRef veAdjustment hoAdjustment

  onButtonPress drawArea $ \event -> do
    readIORef posRef >>= clearCursor textArea
    handleButtonPress textArea (eventX event,eventY event) >>= writeIORef posRef
    return $ Events.eventSent event

  onKeyPress drawArea $ \event -> do
    let drawArea = drawingArea textArea
        posRef   = currentPosition textArea
        areaRef  = textAreaContent textArea
    readIORef posRef >>= clearCursor textArea
    pos@(x,y) <- readIORef posRef
    let key = Events.eventKeyName event
    listBefore <- (readIORef areaRef >>= \areaContent -> generateContentList areaContent (\_ -> True))
    char <- getCharacter event
    when (isJust char) $ displayCharacter textArea (ContentEntry pos $ fromJust char) >>= writeIORef posRef
    content <- readIORef areaRef
    let hAdj = hAdjustment textArea
        vAdj = vAdjustment textArea

    actions <- KeyHandler.handleKey content pos key hAdj vAdj
    runActions textArea actions

    listAfter <- (readIORef areaRef >>= \areaContent -> generateContentList areaContent (\_ -> True))
    
    HIGH.highlight areaContent
    
    removeContent textArea listBefore
    readIORef posRef >>= redrawDrawingArea textArea listAfter
    return $ Events.eventSent event

  onExpose drawArea $ \(Expose sent _ _ _) -> do
    content <- readIORef areaRef
    list <- generateContentList content (\_ -> True)
    readIORef posRef >>= redrawDrawingArea textArea list
    return sent

  return textArea

runActions _ [] = return ()
runActions textArea (x:xs) = do
  runAction textArea x
  runActions textArea xs

runAction textArea (KeyHandler.UpdatePosition pos) = do
  let posRef = currentPosition textArea
  writeIORef posRef pos
runAction textArea (KeyHandler.ExtendDrawingAreaH x) = do
  let drawArea = drawingArea textArea
      hAdj = hAdjustment textArea
  extendDrawingAreaHorizontally textArea hAdj x
runAction textArea (KeyHandler.ExtendDrawingAreaV y) = do
  let drawArea = drawingArea textArea
      vAdj = vAdjustment textArea
  extendDrawingAreaVertically textArea vAdj y
  

initTextArea :: IO(TextArea)
initTextArea = do
  areaContent <- TAC.init 0 0
  textArea <- initTextAreaWithContent areaContent
  return $ textArea

setUpDrawingArea :: IO (DrawingArea)
setUpDrawingArea = do
  drawingArea <- drawingAreaNew
  widgetModifyBg drawingArea StateNormal defaultBgColor
  set drawingArea [widgetCanFocus := True]
  widgetSetSizeRequest drawingArea (round width) (round height)
  return drawingArea

removeContent _ [] = return ()
removeContent textArea ((pos,_):xs) = do
  removeCharacter textArea pos
  removeContent textArea xs

getCharacter :: Event -> IO (Maybe Char)
getCharacter event = 
  let val = Events.eventKeyVal event
  in return $ keyToChar val

handleButtonPress :: TextArea -> Position -> IO (Position)  
handleButtonPress textArea pos = do
  newPos <- updatePosition pos 
  showCursor textArea newPos
  return newPos
    where
      drawArea = drawingArea textArea

renderScene :: TextArea -> ContentEntry -> RGBColor -> IO ()
renderScene textArea (ContentEntry (x,y) char) (RGBColor r g b) = do
  let drawArea = drawingArea textArea
  removeCharacter textArea (x,y)
  drawWindow <- widgetGetDrawWindow drawArea
  renderWithDrawable drawWindow $ do 
    setSourceRGBA r g b 1.0
    moveTo (x+2) (y+hef-3)
    setFontSize 14
    showText [char]

removeCharacter :: TextArea -> Position -> IO ()     
removeCharacter textArea (x,y) = do
  let drawArea = drawingArea textArea
  drawWindow <- widgetGetDrawWindow drawArea
  drawWindowClearArea drawWindow (round x) (round y) (round bef) (round hef) 


redrawCharacters :: TextArea -> ContentList -> Double -> Double -> IO ()
redrawCharacters _ [] _ _ = return ()
redrawCharacters textArea ((pos@(x,y),char):xs) bef hef = do
  redrawCharacters textArea xs bef hef
  removeCharacter textArea (x,y)								
  renderScene textArea (ContentEntry (x+bef,y+hef) char) black	

redrawCharactersByCoordinates :: TextArea -> Position -> ContentList -> IO ()  
redrawCharactersByCoordinates _ _ [] = return ()  
redrawCharactersByCoordinates textArea (x1,y1) (((x2,y2),char):xs) = do
  removeCharacter textArea (x2,y2)
  renderScene textArea (ContentEntry (x2+x1,y1) char) black
  redrawCharactersByCoordinates textArea (x1,y1) xs

redrawDrawingArea :: TextArea -> ContentList -> Position -> IO ()
redrawDrawingArea textArea content pos = do
  showCursor textArea pos
  redrawContent textArea content

redrawContent :: TextArea -> ContentList -> IO ()  
redrawContent _ [] = return ()
redrawContent textArea list@((pos,char):xs) = do
  let areaContentRef = textAreaContent textArea
  areaContent <- readIORef areaContentRef
  mayCell <- getCell areaContent pos
  let color = if (isNothing mayCell) then TAC.defaultColor else (snd $ fromJust $ mayCell)   
  renderScene textArea (ContentEntry pos char) color
  redrawContent textArea xs

showCursor :: TextArea -> Position -> IO ()
showCursor textArea (x,y) = do
  let drawArea = drawingArea textArea
  drawWindow <- widgetGetDrawWindow drawArea
  gc <- gcNew drawWindow
  gcSetValues gc $ newGCValues { foreground = defaultCursorColor }
  drawRectangle drawWindow gc True (round x) (round y) 2 (round hef)
  return ()

clearCursor :: TextArea -> Position -> IO ()
clearCursor textArea (x,y) = do
  let drawArea = drawingArea textArea
  drawWindow <- widgetGetDrawWindow drawArea
  drawWindowClearArea drawWindow (round x) (round y) 2 (round hef) 

displayCharacter :: TextArea -> ContentEntry -> IO (Position)
displayCharacter textArea entry@(ContentEntry (x,y) char) = do
  areaContent <- readIORef areaRef
  addCharacter areaContent entry
  renderScene textArea entry black       -- GUI
  extendDrawingAreaHorizontally textArea hoAdjustment x  -- GUI 	
  return (x+bef,y)
    where
      drawArea = drawingArea textArea
      areaRef = textAreaContent textArea
      hoAdjustment = hAdjustment textArea

updateDirection :: Double -> Double -> Adjustment -> IO (Double)
updateDirection direction size adjustment = do
  if (direction >= size) 
  then do
    value <- adjustmentGetValue adjustment
    when (direction - size < value) $ do adjustmentSetValue adjustment $ direction - size
    return $ direction - size
  else return direction

extendDrawingAreaHorizontally :: TextArea -> Adjustment -> Double -> IO ()
extendDrawingAreaHorizontally textArea adjustment x = do 
  let drawArea = drawingArea textArea
  (w,h) <- widgetGetSizeRequest drawArea
  value <- adjustmentGetValue adjustment
  when (width + value - x - bef < bef) $ do
    when (round (x + bef) >= w) $ widgetSetSizeRequest drawArea (w + (round bef)) h
    adjustmentSetValue adjustment $ value + bef

extendDrawingAreaVertically :: TextArea -> Adjustment -> Double -> IO ()
extendDrawingAreaVertically textArea adjustment y = do 
  let drawArea = drawingArea textArea
  (w,h) <- widgetGetSizeRequest drawArea
  value <- adjustmentGetValue adjustment
  when (height + value - y - hef < hef) $ do
    when (round (y + hef) >= h) $ widgetSetSizeRequest drawArea w (h + (round hef))
    adjustmentSetValue adjustment $ value + hef
