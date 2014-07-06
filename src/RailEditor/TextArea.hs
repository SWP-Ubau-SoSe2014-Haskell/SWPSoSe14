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
    currentPosition :: IORef (TAC.Position), --The current position of cursor
    vAdjustment :: GTK.Adjustment, --needed to create a scrolledWindow
    hAdjustment :: GTK.Adjustment } --needed to create a scrolledWindow

--The cursor color
defaultCursorColor :: GC.Color
defaultCursorColor = GC.Color 65535 0 0 

--The background color
defaultBgColor :: GC.Color
defaultBgColor = GC.Color 65535 65535 65535

-- displayed size of TextArea
width = 600
height = 400

bef = 15 :: TAC.Coord --width of a character
hef = 15 :: TAC.Coord --height of a character

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
  let areaRef = textAreaContent textArea
  writeIORef areaRef areaContent

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
  drawArea <- setUpDrawingArea
  GTK.scrolledWindowAddWithViewport scrwin drawArea
  posRef <- newIORef (0,0)
  areaRef <- newIORef areaContent
  let textArea = TA drawArea areaRef scrwin posRef veAdjustment hoAdjustment
  
  {-This function is called when the user press a mouse button.
    It calls the handleButtonPress function.  
  -}
  GTK.onButtonPress drawArea $ \event -> do
    readIORef posRef >>= clearCursor textArea
    handleButtonPress textArea (round(Events.eventX event),round(Events.eventY event)) >>= writeIORef posRef
    return $ Events.eventSent event
    
  {-
    This function is called when the user presses a key.
    It starts the heyHandler the highlighter and redraw the textAreaContent.
  -}
  GTK.onKeyPress drawArea $ \event -> do
    let drawArea = drawingArea textArea
        posRef   = currentPosition textArea
        areaRef  = textAreaContent textArea
    readIORef posRef >>= clearCursor textArea
    pos@(x,y) <- readIORef posRef
    tac <- readIORef areaRef --TextAreaContent
    let
      modif = Events.eventModifier event
      key = Events.eventKeyName event
      val = Events.eventKeyVal event
      modus = "Normal"
    --TODO capture before size and after size and resize the TextArea
    pos <- KeyHandler.handleKey tac pos modus modif key val
    writeIORef posRef pos
   -- runActions textArea actions --On the visible part of TextArea
    HIGH.highlight tac
    showCursor textArea pos
    redrawContent textArea
    return $ Events.eventSent event
    
  {-
    Called when the application starts
    It setting the cursor and draw the textAreaContent
  -}
  GTK.onExpose drawArea $ \(Events.Expose sent _ _ _) -> do
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
  GTK.widgetSetSizeRequest drawingArea width height
  return drawingArea

--This handles a mouse button press event
handleButtonPress :: TextArea -> TAC.Position -> IO (TAC.Position)  
handleButtonPress textArea pos = do
  showCursor textArea pos
  return pos

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
yCoord y = (y*hef)

--This function returns the  coord in relation to drawingArea  
xCoord :: TAC.Coord -> TAC.Coord
xCoord x = (x*bef)
  
-- removes the content from Text Area
removeContent :: TextArea -> IO ()
removeContent textArea = do
  drawWindow <- GTK.widgetGetDrawWindow drawArea
  tac <- readIORef tacIORef
  s@(xMax,yMax) <- TAC.size tac
  checkAndClear drawWindow [0..xMax] [0..yMax]
  where 
    drawArea = drawingArea textArea
    tacIORef = textAreaContent textArea
    -- is there a displayed char?
    haveToClear :: TAC.Position -> IO(Bool)
    haveToClear pos = do 
      tac <- readIORef tacIORef
      cell <- TAC.getCell tac pos
      return $ isNothing(cell)
    -- clear the char at x,y when displayed
    clear :: GTK.DrawWindow  -> TAC.Position -> IO ()
    clear drawWindow (x,y)= do
      htc <- haveToClear (x,y)
      case (not htc) of
        True -> GTK.drawWindowClearArea drawWindow (fromIntegral $ xCoord x) 
          (fromIntegral(y*hef)) (fromIntegral bef) (fromIntegral hef)
        _ -> return ()
    -- Call clear on every possible coord 
    checkAndClear ::  GTK.DrawWindow -> [TAC.Coord] -> [TAC.Coord] -> IO ()
    checkAndClear _ [] _ = return ()
    checkAndClear drawWindow (x:xs) ys= do
      checkAndClearHelp drawWindow x ys
      checkAndClear drawWindow xs ys
    checkAndClearHelp :: GTK.DrawWindow -> TAC.Coord -> [TAC.Coord] -> IO ()
    checkAndClearHelp _ _ [] = return ()
    checkAndClearHelp drawWindow x (y:ys) = do
      clear drawWindow (x,y)
      checkAndClearHelp drawWindow x ys

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

-- This Function draws every character in TAC.
redrawContent :: TextArea -> IO ()  
redrawContent textArea = do
  let tacIORef = textAreaContent textArea
  tac <- readIORef tacIORef
  s@(xMax,yMax) <- TAC.size tac
  checkAndDraw tac [0..xMax] [0..yMax]
  where
    -- Call renderScene to 
    draw:: TAC.TextAreaContent -> TAC.Position -> IO()
    draw tac (x,y) = do
      mayCell <- TAC.getCell tac (x,y)
      if (isNothing mayCell) 
      then return()
      else do
        let
          col = snd $ fromJust $ mayCell
          char = fst $ fromJust $ mayCell
        renderScene textArea (x,y) char col
      return ()
    --This call draw on every possible coord  
    checkAndDraw :: TAC.TextAreaContent -> [TAC.Coord] -> [TAC.Coord] -> IO ()
    checkAndDraw _ [] _ = return ()
    checkAndDraw tac (x:xs) ys = do
      checkAndDrawHelp tac x ys
      checkAndDraw tac xs ys
    checkAndDrawHelp :: TAC.TextAreaContent -> TAC.Coord -> [TAC.Coord] -> IO ()
    checkAndDrawHelp _ _ [] = return ()
    checkAndDrawHelp tac x (y:ys) = do
      draw tac (x,y)
      checkAndDrawHelp tac x ys
      
--Draws a cursor at the position.
--The function adds the TACU offsets
showCursor :: TextArea -> TAC.Position -> IO ()
showCursor textArea (x,y) = do
  let drawArea = drawingArea textArea
  drawWindow <- GTK.widgetGetDrawWindow drawArea
  gc <- GC.gcNew drawWindow
  GC.gcSetValues gc $ GC.newGCValues { GC.foreground = defaultCursorColor }
  GTK.drawRectangle drawWindow gc True x y 2 hef
  return ()

-- delets the cursor at the given position.
clearCursor :: TextArea -> TAC.Position -> IO ()
clearCursor textArea (x,y) = do
  let drawArea = drawingArea textArea
  drawWindow <- GTK.widgetGetDrawWindow drawArea
  GTK.drawWindowClearArea drawWindow (fromIntegral x) (fromIntegral y) 2 (fromIntegral(hef))
{-
  This function extends the TextAre horizontal.
  It should be called after KeyHandler 
  if prev TAC.size tac < after TAC.size tac
-}
extendDrawingAreaHorizontally :: TextArea -> GTK.Adjustment -> TAC.Coord -> IO ()
extendDrawingAreaHorizontally textArea adjustment x = do 
  let drawArea = drawingArea textArea
  (w,h) <- GTK.widgetGetSizeRequest drawArea
  value <- GTK.adjustmentGetValue adjustment
  when (width + round(value) - x - bef < bef) $ do
    when (x + bef >= w) $ GTK.widgetSetSizeRequest drawArea (w + bef) h
    GTK.adjustmentSetValue adjustment $ value + (fromIntegral bef)

{-
  This function extends the TextAre vertically.
  It should be called after KeyHandler 
  if prev TAC.size tac < after TAC.size tac
-}
extendDrawingAreaVertically :: TextArea -> GTK.Adjustment -> TAC.Coord -> IO ()
extendDrawingAreaVertically textArea adjustment y = do 
  let drawArea = drawingArea textArea
  (w,h) <- GTK.widgetGetSizeRequest drawArea
  value <- GTK.adjustmentGetValue adjustment
  when (height + round(value) - y - hef < hef) $ do
    when (y + hef >= h) $ GTK.widgetSetSizeRequest drawArea w (h + hef)
    GTK.adjustmentSetValue adjustment $ value + (fromIntegral hef)
