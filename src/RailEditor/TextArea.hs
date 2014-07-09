{- |
Module      :  TextArea.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT
Stability   :  experimental

The TextArea-module depicts the view on the data structure stored in the TextAreaContent-module for the editor.
-}


module TextArea(

-- * Types
  TextArea,
-- * Constructors
  initTextAreaWithContent,
  initTextArea,
-- * Constants
-- * Methods
  textAreaContent,
  setTextAreaContent,
  getTextAreaContainer -- This function should be used to get a widget to place in MainWindow
  )where
    
    -- imports --
import qualified TextAreaContent as TAC
import qualified KeyHandler
import qualified Highlighter as HIGH

import qualified Graphics.Rendering.Cairo as CAIRO
import qualified Graphics.UI.Gtk as GTK
import qualified Graphics.UI.Gtk.Gdk.Events as Events
import Control.Concurrent (threadDelay)
import Data.IORef as IORef
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import qualified Graphics.UI.Gtk.Gdk.GC as GC

{-
  This is the main datatype of TextArea 
-}
data TextArea = TA { drawingArea :: GTK.DrawingArea,
    textAreaContent :: IORef (TAC.TextAreaContent), --The TAC
    scrolledWindow :: GTK.ScrolledWindow, --The part to bind it in a container
    currentPosition :: IORef (TAC.Position), --The current position of cursor without hef and bef
    vAdjustment :: GTK.Adjustment, --needed to create a scrolledWindow
    hAdjustment :: GTK.Adjustment } --needed to create a scrolledWindow

--The cursor color
defaultCursorColor :: GC.Color
defaultCursorColor = GC.Color 65535 0 0 

--The background color
defaultBgColor :: GC.Color
defaultBgColor = GC.Color 65535 65535 65535

bef = 15 :: TAC.Coord --width of a character
hef = 15 :: TAC.Coord --height of a character

width = 400
height = 600

{-
  This function returns a scrolled Window which can be used
  to bind it on a widget.
-}
getTextAreaContainer :: TextArea -> IO (GTK.ScrolledWindow)
getTextAreaContainer textArea = return $ scrolledWindow textArea

{-
  This function sets the TextArea TAC to the TAC given in argument 2.
-}
setTextAreaContent :: TextArea -> TAC.TextAreaContent -> IO ()
setTextAreaContent textArea areaContent= do
  let 
    areaRef = textAreaContent textArea
    drawArea = drawingArea textArea
  writeIORef areaRef areaContent
  --expand the drawWindow when needed
  tac <- readIORef areaRef
  s@(xMax,yMax) <- TAC.size tac
  (w,h) <- GTK.widgetGetSizeRequest drawArea
  if((yMax*hef) > h ) && ((xMax*bef) > w)
  then GTK.widgetSetSizeRequest drawArea (w + (xMax*bef-w)) (h+((yMax*hef)-w))
  else do
    when ((yMax*hef) > h ) $
      GTK.widgetSetSizeRequest drawArea w (h+((yMax*hef)-w))
    when ((xMax*bef) > w) $
      GTK.widgetSetSizeRequest drawArea (w + (xMax*bef-w))  h
  HIGH.highlight areaContent
  redrawContent textArea

{- 
  This Function calls initTextAreaWithContent with an empty TAC
-}
initTextArea :: IO(TextArea)
initTextArea = do
  areaContent <- TAC.init 0 0
  textArea <- initTextAreaWithContent areaContent
  return $ textArea

{-
  This function setup the TextArea. It Set up the drawWindow 
  related to textAreaContent. It also setup the different user events
  for editing the TextArea
-}
initTextAreaWithContent :: TAC.TextAreaContent -> IO (TextArea)
initTextAreaWithContent areaContent = do
  veAdjustment <- GTK.adjustmentNew 0 0 0 (fromIntegral hef) 0 0
  hoAdjustment <- GTK.adjustmentNew 0 0 0 (fromIntegral bef) 0 0
  scrwin <- GTK.scrolledWindowNew (Just hoAdjustment) (Just veAdjustment)
  areaRef <- newIORef areaContent
  drawArea <- setUpDrawingArea
  GTK.scrolledWindowAddWithViewport scrwin drawArea
  posRef <- newIORef (0,0)
  let textArea = TA drawArea areaRef scrwin posRef veAdjustment hoAdjustment
  
  {-This function is called when the user press a mouse button.
    It calls the handleButtonPress function.  
  -}
  GTK.onButtonPress drawArea $ \event -> do
    let posRef = currentPosition textArea
    GTK.widgetGrabFocus drawArea
    readIORef posRef >>= clearCursor textArea
    handleButtonPress textArea (round(Events.eventX event),round(Events.eventY event)) >>= writeIORef posRef
    return $ Events.eventSent event
    
  {-
    This function is called when the user presses a key.
    It starts the heyHandler the highlighter and redraw the textAreaContent.
  -}
  GTK.onKeyPress drawArea $ \event -> do
    let 
      posRef   = currentPosition textArea
      areaRef  = textAreaContent textArea
      modif = Events.eventModifier event
      key = Events.eventKeyName event
      val = Events.eventKeyVal event
      modus = "Insert"
    tac <- readIORef areaRef --TextAreaContent
    readIORef posRef >>= clearCursor textArea
    pos@(x,y) <- readIORef posRef
    pos@(kx,ky) <- KeyHandler.handleKey tac pos modus modif key val
 
    --expand the drawWindow when needed
    extendDrawingAreaHorizontally textArea (kx)
    extendDrawingAreaVertically textArea (ky)
      
    writeIORef posRef pos
    
    HIGH.highlight tac
    
    redrawContent textArea
    showCursor textArea pos
    return $ Events.eventSent event
 
  {-
    Called when the application starts
    It setting the cursor and draw the textAreaContent
  -}
  GTK.onExpose drawArea $ \(Events.Expose sent _ _ _) -> do
    let 
      posRef   = currentPosition textArea
      areaRef  = textAreaContent textArea
    content <- readIORef areaRef
    list <- TAC.generateContentList content (\_ -> True)
    pos <- readIORef posRef
    showCursor textArea pos
    redrawContent textArea
    return sent
  return textArea


{-
  This function setup the DrawArea using GTK.DrawingArea.
-}
setUpDrawingArea :: IO (GTK.DrawingArea)
setUpDrawingArea = do
  drawingArea <- GTK.drawingAreaNew
  GTK.widgetModifyBg drawingArea GTK.StateNormal defaultBgColor
  GTK.set drawingArea [GTK.widgetCanFocus GTK.:= True]
  GTK.widgetSetSizeRequest drawingArea (width) (height)
  return drawingArea

--This handles a mouse button press event
handleButtonPress :: TextArea -> TAC.Position -> IO (TAC.Position)  
handleButtonPress textArea (x,y) = do
  let curPosIORef = currentPosition textArea
  showCursor textArea newPos
  return newPos
  where newPos = ((div x bef),(div y hef))-- position without hef and bef

{-
  This function renders a character at the given position.
  It calculate the coords for drawingArea from TAC.Position.
-} 
renderScene :: TextArea -> TAC.Position -> Char -> TAC.RGBColor -> IO ()
renderScene textArea (x,y) char (TAC.RGBColor r g b) = do
  let drawArea = drawingArea textArea
  removeCharacter textArea (x,y)
  drawWindow <- GTK.widgetGetDrawWindow drawArea
  GTK.renderWithDrawable drawWindow $ do 
    CAIRO.setSourceRGBA r g b 1.0
    CAIRO.moveTo (fromIntegral (xCoord x)) (fromIntegral (yCoord y))
    CAIRO.setFontSize 14
    CAIRO.showText [char]

--This function returns the y coord in relation to drawingArea    
yCoord :: TAC.Coord -> TAC.Coord
yCoord 0 = hef
yCoord y = (y*hef)+hef

--This function returns the  coord in relation to drawingArea  
xCoord :: TAC.Coord -> TAC.Coord
xCoord x = (x*bef)
  
-- removes the content from Text Area which is displayed in scrollable Frame
removeContent :: TextArea -> IO ()
removeContent textArea = do
  let 
    drawArea = drawingArea textArea
    vAdj = vAdjustment textArea 
    hAdj = hAdjustment textArea
  vAdjValue <- GTK.adjustmentGetValue vAdj
  hAdjValue <- GTK.adjustmentGetValue hAdj
  drawWindow <- GTK.widgetGetDrawWindow drawArea
  s@(w,h) <- GTK.drawableGetSize drawWindow
  GTK.drawWindowClearArea drawWindow (round hAdjValue) (round vAdjValue) w h
 
{-
  This Function removes the caracter at given position
  The function should be called in renderScene to avoid overwriting.
-}
removeCharacter :: TextArea -> TAC.Position -> IO ()     
removeCharacter textArea (x,y) = do
  let drawArea = drawingArea textArea
  drawWindow <- GTK.widgetGetDrawWindow drawArea
  GTK.drawWindowClearArea drawWindow (fromIntegral $ xCoord x) 
          (fromIntegral(y*hef)) (fromIntegral bef) (fromIntegral hef) 

-- This Function draws every character in scrollable Frame.
redrawContent :: TextArea -> IO ()  
redrawContent textArea = do
  removeContent textArea
  let 
    tacIORef = textAreaContent textArea
    drawArea = drawingArea textArea
    vAdj = vAdjustment textArea 
    hAdj = hAdjustment textArea
  vAdjValue <- GTK.adjustmentGetValue vAdj
  hAdjValue <- GTK.adjustmentGetValue hAdj
  drawFrameHeight <- GTK.adjustmentGetPageSize vAdj
  drawFrameWidth <- GTK.adjustmentGetPageSize hAdj
  let 
    xFrom = div (round hAdjValue) bef
    yFrom = div (round vAdjValue) hef
    xTo =  div ((round hAdjValue)+(round drawFrameWidth)) bef
    yTo = div ((round vAdjValue)+(round drawFrameHeight)) hef
  tac <- readIORef tacIORef
  --Function from Control.Monad Monad m => [a] -> (a -> m b) -> m ()
  conPos <- TAC.getOccupiedPositions tac
  forM_ [xFrom..xTo] (\x -> forM_ [yFrom..yTo] (\y -> draw textArea (x,y)))
  where
    -- Call renderScene to 
    draw:: TextArea -> TAC.Position -> IO()
    draw textArea (x,y) = do
      let tacIORef = textAreaContent textArea
      tac <- readIORef tacIORef
      mayCell <- TAC.getCell tac (x,y)
      if (isNothing mayCell) 
      then return()
      else do
        let
          col = snd $ fromJust $ mayCell
          char = fst $ fromJust $ mayCell
        renderScene textArea (x,y) char col
        return ()
        

--Draws a cursor at the position.
--The function adds the TACU offsets
showCursor :: TextArea -> TAC.Position -> IO ()
showCursor textArea (x,y) = do
  let drawArea = drawingArea textArea
  drawWindow <- GTK.widgetGetDrawWindow drawArea
  gc <- GC.gcNew drawWindow
  GC.gcSetValues gc $ GC.newGCValues { GC.foreground = defaultCursorColor }
  GTK.drawRectangle drawWindow gc True (curX (x*bef)) (curY (y*hef)) 2 hef
  return ()

-- delets the cursor at the given position.
clearCursor :: TextArea -> TAC.Position -> IO ()
clearCursor textArea (x,y) = do
  let drawArea = drawingArea textArea
  drawWindow <- GTK.widgetGetDrawWindow drawArea
  GTK.drawWindowClearArea drawWindow 
    (fromIntegral (curX (x*bef))) 
    (fromIntegral (curY (y*hef)))
    2
    (fromIntegral(hef))

--Coords for the cursor to fit the cells of characters
curX x = abs(x-(mod x bef)-1)
curY y = y-(mod y hef)

{-
  This function extends the TextArea horizontal and set the scroll Frame.
  It should be called after KeyHandler 
-}
extendDrawingAreaHorizontally :: TextArea -> TAC.Coord -> IO ()
extendDrawingAreaHorizontally textArea x = do 
  let 
    drawArea = drawingArea textArea
    adjustment = hAdjustment textArea
  width <- GTK.adjustmentGetPageSize adjustment
  (w,h) <- GTK.widgetGetSizeRequest drawArea
  value <- GTK.adjustmentGetValue adjustment
  when ((x*bef) + bef >= w) $
    GTK.widgetSetSizeRequest drawArea (w + bef) h
  when ((x*bef)+bef >= (round width)+(round value)) $
    GTK.adjustmentSetValue adjustment $ value + (fromIntegral bef)
  when ((x*bef)-bef < (round value)) $
    GTK.adjustmentSetValue adjustment $ value - (fromIntegral bef)
    
{-
  This function extends the TextArea vertically and set the scroll Frame.
  It should be called after KeyHandler 
-}
extendDrawingAreaVertically :: TextArea -> TAC.Coord -> IO ()
extendDrawingAreaVertically textArea y = do 
  let 
    drawArea = drawingArea textArea
    adjustment = vAdjustment textArea
  height <- GTK.adjustmentGetPageSize adjustment
  (w,h) <- GTK.widgetGetSizeRequest drawArea
  value <- GTK.adjustmentGetValue adjustment
  when ((y*hef) + hef >= h) $
    GTK.widgetSetSizeRequest drawArea w (h + hef)
  when ((y*hef)+hef >= (round height)+(round value)) $
    GTK.adjustmentSetValue adjustment $ value + (fromIntegral hef)
  when ((y*hef)-hef < (round value)) $
    GTK.adjustmentSetValue adjustment $ value - (fromIntegral hef)
