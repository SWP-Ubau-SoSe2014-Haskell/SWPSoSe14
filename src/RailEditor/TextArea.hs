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
-- * Constants
-- * Methods
  textAreaContent,
  currentPosition,
  setTextAreaContent,
  drawingArea,
  getTextAreaContainer, -- This function should be used to get a widget to place in MainWindow
  setInputMode,
  setHighlighting,
  redrawContent
  )where
    
    -- imports --
import qualified TextAreaContent as TAC
import qualified KeyHandler
import qualified Highlighter as HIGH
import qualified Selection

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
    textAreaContent :: IORef TAC.TextAreaContent, --The TAC
    scrolledWindow :: GTK.ScrolledWindow, --The part to bind it in a container
    currentPosition :: IORef TAC.Position, --The current position of cursor without hef and bef
    vAdjustment :: GTK.Adjustment, --needed to create a scrolledWindow
    hAdjustment :: GTK.Adjustment, --needed to create a scrolledWindow
    inputMode :: IORef KeyHandler.InputMode, -- needed to set and get the current input mode
    getHighlighted :: IORef Bool} -- needed to determine, whether the area is highlighted

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
getTextAreaContainer :: TextArea -> IO GTK.ScrolledWindow
getTextAreaContainer textArea = return $ scrolledWindow textArea

setInputMode :: TextArea -> KeyHandler.InputMode -> IO ()
setInputMode area mode = do
  let modeRef = inputMode area
  writeIORef modeRef mode

{-
  This function sets the TextArea TAC to the TAC given in argument 2.
-}
setTextAreaContent :: TextArea -> TAC.TextAreaContent -> IO ()
setTextAreaContent textArea areaContent= do
  let 
    areaRef = textAreaContent textArea
    drawArea = drawingArea textArea
    highlightedIORef = getHighlighted textArea
  writeIORef areaRef areaContent
  highlighted <- readIORef highlightedIORef
  --expand the drawWindow when needed
  tac <- readIORef areaRef
  s@(xMax,yMax) <- TAC.size tac
  (w,h) <- GTK.widgetGetSizeRequest drawArea
  if yMax*hef > h && xMax*bef > w
  then GTK.widgetSetSizeRequest drawArea (w + (xMax*bef-w)) (h+(yMax*hef-w))
  else do
    when (yMax*hef > h ) $
      GTK.widgetSetSizeRequest drawArea w (h+((yMax*hef)-w))
    when (xMax*bef > w) $
      GTK.widgetSetSizeRequest drawArea (w + (xMax*bef-w))  h
  when highlighted $ HIGH.highlight tac
  redrawContent textArea

-- | sets (True) or unsets (False) the highlighting 
setHighlighting :: TextArea -> Bool -> IO ()
setHighlighting area val = do
  writeIORef (getHighlighted area) val
  tac <- readIORef $ textAreaContent area
  if val
  then HIGH.highlight tac
  else TAC.deleteColors tac
  redrawContent area

{-
  This function setup the TextArea. It Set up the drawWindow 
  related to textAreaContent. It also setup the different user events
  for editing the TextArea
-}
initTextAreaWithContent :: TAC.TextAreaContent -> IO TextArea
initTextAreaWithContent areaContent = do
  veAdjustment <- GTK.adjustmentNew 0 0 0 (fromIntegral hef) 0 0
  hoAdjustment <- GTK.adjustmentNew 0 0 0 (fromIntegral bef) 0 0
  scrwin <- GTK.scrolledWindowNew (Just hoAdjustment) (Just veAdjustment)
  areaRef <- newIORef areaContent
  drawArea <- setUpDrawingArea
  GTK.scrolledWindowAddWithViewport scrwin drawArea
  posRef <- newIORef (0,0)
  modRef <- newIORef KeyHandler.Insert
  highlighted <- newIORef True
  let textArea = TA drawArea areaRef scrwin posRef veAdjustment hoAdjustment modRef highlighted

  {-This function is called when the user press a mouse button.
    It calls the handleButtonPress function.  
  -}
  GTK.onButtonPress drawArea $ \event -> do
    let posRef = currentPosition textArea
    GTK.widgetGrabFocus drawArea
    readIORef posRef >>= clearCursor textArea
    handleButtonPress textArea (round(Events.eventX event),round(Events.eventY event)) >>= writeIORef posRef
    return $ Events.eventSent event
    
  GTK.on drawArea GTK.motionNotifyEvent $ do
    actualPos <- GTK.eventCoordinates
    CAIRO.liftIO $ do
      let areaRef = textAreaContent textArea
      tac <- readIORef areaRef
      currentPos <- readIORef posRef                            
      newPos <- updatePosition actualPos
      clearCursor textArea currentPos
      showCursor textArea newPos
      when (currentPos /= newPos) $ do
        (isAlreadySelected,selectedContent) <- Selection.handleSelection tac currentPos newPos
        if isAlreadySelected 
        then deselectEntries textArea tac selectedContent
        else selectEntries textArea selectedContent
        writeIORef posRef newPos
    GTK.eventRequestMotions
    return True
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
      modRef = inputMode textArea
    modus <- readIORef modRef
    tac <- readIORef areaRef --TextAreaContent
    readIORef posRef >>= clearCursor textArea
    posBef@(x,y) <- readIORef posRef
    pos@(kx,ky) <- KeyHandler.handleKey tac posBef modus modif key val
    --expand the drawWindow when needed
    extendDrawingAreaHorizontally textArea kx
    extendDrawingAreaVertically textArea ky
    writeIORef posRef pos
    highlighted <- readIORef (getHighlighted textArea)
    when highlighted $ HIGH.highlight tac
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
    redrawContent textArea
    readIORef posRef >>= showCursor textArea
    return sent
  return textArea


{-
  This function setup the DrawArea using GTK.DrawingArea.
-}
setUpDrawingArea :: IO GTK.DrawingArea
setUpDrawingArea = do
  drawingArea <- GTK.drawingAreaNew
  GTK.widgetModifyBg drawingArea GTK.StateNormal defaultBgColor
  GTK.set drawingArea [GTK.widgetCanFocus GTK.:= True]
  GTK.widgetAddEvents drawingArea [GTK.ButtonMotionMask]
  GTK.widgetSetSizeRequest drawingArea width height
  return drawingArea

--This handles a mouse button press event
handleButtonPress :: TextArea -> TAC.Position -> IO TAC.Position
handleButtonPress textArea (x,y) = do
  let tacIORef = textAreaContent textArea
      curPosIORef = currentPosition textArea
  showCursor textArea newPos
  tac <- readIORef tacIORef
  selection <- TAC.getSelectedPositons tac
  deselectEntries textArea tac selection
  Selection.updateCells tac selection False
  return newPos
  where newPos = (div x bef,div y hef)-- position without hef and bef

{-
  This function renders a character at the given position.
  It calculate the coords for drawingArea from TAC.Position.
-} 
renderScene :: TextArea -> TAC.Position -> Char -> TAC.RGBColor -> Bool -> Bool -> IO ()
renderScene textArea (x,y) char (TAC.RGBColor r g b) breakpoint isIP = do
  let drawArea = drawingArea textArea
  removeCharacter textArea (x,y)
  drawWindow <- GTK.widgetGetDrawWindow drawArea
  GTK.renderWithDrawable drawWindow $ do 
    CAIRO.setSourceRGBA r g b 1.0
    CAIRO.moveTo (fromIntegral (xCoord x + 2)) (fromIntegral (yCoord y - 3))
    CAIRO.setFontSize 14
    CAIRO.showText [char]
  gc <- GC.gcNew drawWindow
  GC.gcSetValues gc $ GC.newGCValues { GC.foreground = defaultCursorColor }
  when breakpoint $ GTK.drawRectangle drawWindow gc False (xCoord x-2) (yCoord (y-1)) (bef-2) (hef-2)
  when isIP $ GTK.drawArc drawWindow gc False (xCoord x-2) (yCoord (y-1)) (bef-2) (hef-2) 0 23040

--This function returns the y coord in relation to drawingArea    
yCoord :: TAC.Coord -> TAC.Coord
yCoord 0 = hef
yCoord y = y*hef + hef

--This function returns the  coord in relation to drawingArea  
xCoord :: TAC.Coord -> TAC.Coord
xCoord x = x*bef
  
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
  let 
    tacIORef = textAreaContent textArea
    drawArea = drawingArea textArea
    vAdj = vAdjustment textArea 
    hAdj = hAdjustment textArea
  vAdjValue <- GTK.adjustmentGetValue vAdj
  hAdjValue <- GTK.adjustmentGetValue hAdj
  drawFrameHeight <- GTK.adjustmentGetPageSize vAdj
  drawFrameWidth <- GTK.adjustmentGetPageSize hAdj
  drawWindow <- GTK.widgetGetDrawWindow drawArea
  s@(w,h) <- GTK.drawableGetSize drawWindow
  GTK.drawWindowClearArea drawWindow (round hAdjValue) (round vAdjValue) w h
  let 
    xFrom = div (round hAdjValue) bef
    yFrom = div (round vAdjValue) hef
    xTo =  div (round hAdjValue + round drawFrameWidth) bef
    yTo = div (round vAdjValue + round drawFrameHeight) hef
  tac <- readIORef tacIORef
  --Function from Control.Monad Monad m => [a] -> (a -> m b) -> m ()
  forM_ [xFrom..xTo] (\x -> forM_ [yFrom..yTo] (\y -> draw textArea (x,y)))
  where
    -- Call renderScene to 
    draw:: TextArea -> TAC.Position -> IO()
    draw textArea pos = do
      let tacIORef = textAreaContent textArea
      tac <- readIORef tacIORef
      mayCell <- TAC.getCell tac pos
      unless (isNothing mayCell) $ do 
        let ((char,isSelected), color) = fromJust mayCell
        cnt <- readIORef (TAC.context tac)
        renderScene textArea pos char color (Map.findWithDefault False pos (TAC.breakMap cnt)) (pos == TAC.curIPPos cnt)
        when isSelected $ selectEntry textArea pos

--Draws a cursor at the position.
--The function adds the TACU offsets
showCursor :: TextArea -> TAC.Position -> IO ()
showCursor textArea (x,y) = do
  let drawArea = drawingArea textArea
  drawWindow <- GTK.widgetGetDrawWindow drawArea
  gc <- GC.gcNew drawWindow
  GC.gcSetValues gc $ GC.newGCValues { GC.foreground = defaultCursorColor }
  GTK.drawRectangle drawWindow gc True (curX (x*bef)) (curY (y*hef)) 2 hef
  --http://hackage.haskell.org/package/gtk-0.12.5.7/docs/Graphics-UI-Gtk-Gdk-Drawable.html#t:Drawable
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
    (fromIntegral hef)

--Coords for the cursor to fit the cells of characters
curX x = abs(x - mod x bef-1)
curY y = y - mod y hef

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
  when ((x*bef)+bef >= w) $
    GTK.widgetSetSizeRequest drawArea ((x*bef)+bef) h
  when (x*bef +bef >= round width + round value || x*bef-bef < round value) $
    GTK.adjustmentSetValue adjustment $ fromIntegral (x*bef)
    
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
  when (y*hef + hef >= h) $
    GTK.widgetSetSizeRequest drawArea w (y*hef + hef)
  when (y*hef+hef >= round height + round value) $
    GTK.adjustmentSetValue adjustment $ value + fromIntegral hef
  when (y*hef-hef < round value) $
    GTK.adjustmentSetValue adjustment $ value - fromIntegral hef


selectEntries :: TextArea -> [TAC.Position] -> IO ()
selectEntries _ [] = return ()
selectEntries textArea (x:xs) = do
  selectEntry textArea x
  selectEntries textArea xs

selectEntry :: TextArea -> TAC.Position -> IO ()
selectEntry textArea (x,y) = drawSelection textArea TAC.black 0.1 (x,y) (fromIntegral bef) (fromIntegral hef) 

deselectEntries :: TextArea -> TAC.TextAreaContent -> [TAC.Position] -> IO ()
deselectEntries _ _ [] = return ()
deselectEntries textArea tac (x:xs) = do
  deselectEntry textArea tac x
  deselectEntries textArea tac xs
    
deselectEntry :: TextArea -> TAC.TextAreaContent -> TAC.Position -> IO ()
deselectEntry textArea tac pos  = do
  drawSelection textArea TAC.white 1 pos (bef+2) hef 
  maybeCell <- TAC.getCell tac pos
  let ((char,isSelected),color) = fromJust maybeCell
  cnt <- readIORef (TAC.context tac)
  renderScene textArea pos char color (Map.findWithDefault False pos (TAC.breakMap cnt)) (pos == TAC.curIPPos cnt)

drawSelection :: TextArea -> TAC.RGBColor -> Double -> TAC.Position -> TAC.Coord -> TAC.Coord -> IO () 
drawSelection textArea (TAC.RGBColor r g b) alpha (x,y) width height = do
  drawWindow <- GTK.widgetGetDrawWindow $ drawingArea textArea
  GTK.renderWithDrawable drawWindow $ do
    CAIRO.setSourceRGBA r g b alpha
    CAIRO.rectangle (fromIntegral (xCoord x)) (fromIntegral (hef * y)) (fromIntegral width) (fromIntegral height)
    CAIRO.fill

updatePosition :: (Double,Double) -> IO TAC.Position
updatePosition (x,y) = 
  return (round leftX,round topY)
  where leftX = getMultiplier x (fromIntegral bef) 0
        topY  = getMultiplier y (fromIntegral hef) 0
  
getMultiplier :: Double -> Double -> Double -> Double
getMultiplier a b count
  | a > b * (count+1) = getMultiplier a b (count+1)
  | otherwise = count

