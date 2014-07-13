{- |
Module      :  KeyHandler.hs
Description :  .
Maintainer  :  Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  stable

The KeyHandler-module allows to react on keypress-events in the editor.
-}
module KeyHandler (
                   handleKey,   -- handles keypress-events
                   InputMode(Insert, Replace,Smart)
                  )
  where

import Graphics.UI.Gtk
import Control.Monad
import Data.Maybe
import qualified TextAreaContent as TAC
import qualified TextAreaContentUtils as TACU
import qualified RedoUndo as History

data InputMode = Replace | Insert | Smart

-- | handleKey passes key depending on Entrymode and handles RedoUndo Shortcuts.
handleKey :: TAC.TextAreaContent
  -> TAC.Position
  -> InputMode
  -> [Modifier]
  -> String
  -> KeyVal
  -> IO(TAC.Position)
handleKey tac pos modus modif key val = 
  if (elem Control modif && 
    (keyToChar val == Just 'z' || keyToChar val == Just 'Z'))
  then
    if elem Shift modif
    then History.redo tac pos
    else History.undo tac pos
  else
    case modus of
      Insert -> handleKeyIns tac pos modif key val
      Replace -> handleKeyRP tac pos modif key val
      Smart -> handleKeySpec tac pos modif key val

-- | handles keys in Insert-mode
handleKeyIns :: TAC.TextAreaContent
  -> TAC.Position
  -> [Modifier]
  -> String
  -> KeyVal
  -> IO(TAC.Position)
handleKeyIns tac pos@(x,y) modif key val = do
  if ((isJust $ keyToChar val) || key=="dead_circumflex")
  then handlePrintKeyIns tac pos key val
  else do
    if isArrow key
    then handleArrowsIns key pos tac
    else
      case key of
        "BackSpace" -> handleBackSpace tac pos
        "Return" -> handleReturn tac pos
        "Tab" -> handleTab tac pos modif
        "ISO_Left_Tab" -> handleTab tac pos modif
        "Delete" -> handleDelete tac pos
        "Home" -> return (0,y)
        "End" -> do
          finX <- TAC.findLastChar tac y
          return ((if finX==(-1) then 0 else finX+1),y)
        _ -> return pos

-- | handles keys in Replace-mode
handleKeyRP :: TAC.TextAreaContent
  -> TAC.Position
  -> [Modifier]
  -> String
  -> KeyVal
  -> IO(TAC.Position)
handleKeyRP tac pos@(x,y) modif key val =
  if ((isJust $ keyToChar val) || key=="dead_circumflex")
  then handlePrintKeyRP tac pos key val
  else
    if isArrow key
    then handleArrowsIns key pos tac
    else
      case key of
        "BackSpace" -> handleBackSpace tac pos
        "Return" -> handleReturn tac pos
        "Tab" -> handleTab tac pos modif
        "ISO_Left_Tab" -> handleTab tac pos modif
        "Delete" -> handleDelete tac pos
        "Home" -> return (0,y)
        "End" -> do
          finX <- TAC.findLastChar tac y
          return ((if finX==(-1) then 0 else finX+1),y)
        _ -> return pos

-- | handles keys in Smart-mode
handleKeySpec :: TAC.TextAreaContent
  -> TAC.Position
  -> [Modifier]
  -> String
  -> KeyVal
  -> IO(TAC.Position)
handleKeySpec tac pos@(x,y) modif key val =
  if ((isJust $ keyToChar val) || key=="dead_circumflex")
  then handlePrintKeySpec tac pos key val
  else
    if isArrow key
    then
      handleArrowsSpec key pos tac
    else
      case key of
        "BackSpace" -> handleBackSpace tac pos
        "Return" -> handleReturnRail tac pos
        "Tab" -> handleTab tac pos modif
        "ISO_Left_Tab" -> handleTab tac pos modif
        "Delete" -> handleDelete tac pos
        "Home" -> return (0,y)
        "End" -> do
          finX <- TAC.findLastChar tac y
          return (finX+1,y)
        _ -> return pos

-- | handling of printable keys in Replace-mode (overwriting)
handlePrintKeyRP :: TAC.TextAreaContent
  -> TAC.Position
  -> String
  -> KeyVal
  -> IO(TAC.Position)
handlePrintKeyRP tac pos@(x,y) key val = do
  let char = (if key=="dead_circumflex" then '^' else fromJust $ keyToChar val)
  cell <- TAC.getCell tac pos
  let (curchar, _) = if isNothing cell then (TAC.defaultChar, TAC.defaultColor) else fromJust cell
  History.action tac pos (TAC.Replace [curchar] [char])
  TAC.putCell tac pos (char,TAC.defaultColor)
  return (x+1,y)

-- | handling of printable keys in Replace-mode and Smart-mode (overwriting)
handlePrintKeySpec :: TAC.TextAreaContent
  -> TAC.Position
  -> String
  -> KeyVal
  -> IO(TAC.Position)
handlePrintKeySpec tac pos@(x,y) key val = do
  let char = (if key=="dead_circumflex" then '^' else fromJust $ keyToChar val)
  cell <- TAC.getCell tac pos
  dir@(dx,dy) <- TAC.getDirection tac
  if char `elem` "*+x^v><-|/@"
  then do
    let
      newDir@(nx,ny) = getNewDirection char dir
      (curchar, _) = if isNothing cell then (TAC.defaultChar, TAC.defaultColor) else fromJust cell
    History.action tac pos (TAC.Replace [curchar] [char])
    TAC.putCell tac pos (char,TAC.defaultColor)
    return (x+nx,y+ny)
  else do
    let
      newDir@(nx,ny) = dir
      (curchar, _) = if isNothing cell then (TAC.defaultChar, TAC.defaultColor) else fromJust cell
    History.action tac pos (TAC.Replace [curchar] [char])
    TAC.putCell tac pos (char,TAC.defaultColor)
    return (x+nx,y+ny)

-- | handling of printable keys in Insert-mode
handlePrintKeyIns :: TAC.TextAreaContent
  -> TAC.Position
  -> String
  -> KeyVal
  -> IO(TAC.Position)
handlePrintKeyIns tac pos@(x,y) key val = do
  let char = (if key=="dead_circumflex" then '^' else fromJust $ keyToChar val)
  History.action tac pos (TAC.Insert [char])
  TACU.moveChars tac pos (1,0)
  TAC.putCell tac pos (char,TAC.defaultColor)
  return (x+1,y)

-- | checking if key is an arrow 
isArrow :: String
  -> Bool
isArrow key = elem key ["Left", "Right", "Up", "Down"]

-- | handles arrows in Insert-mode and Replace-mode
handleArrowsIns :: String
  -> TAC.Position
  -> TAC.TextAreaContent
  -> IO(TAC.Position)
handleArrowsIns key pos@(x,y) tac = do
  (maxX,maxY) <- TAC.size tac
  case key of
    "Left" ->
      if x==0 && y==0
      then return (x,y)
      else
        if x==0
        then do
          finPrev <- TAC.findLastChar tac (y-1)
          return (finPrev+1,y-1)
        else return (x-1,y)
    "Right" ->
      if x==maxX && y==maxY
      then return(x,y)
      else 
        if x==maxX
        then return (0,y+1)
        else return (x+1,y)
    "Up" ->
      if y==0
      then return (x,y)
      else do
        finPrev <- TAC.findLastChar tac (y-1)
        if finPrev<x
        then return (finPrev+1,y-1)
        else return (x,y-1)
    "Down" ->
      if y==maxY
      then return (x,y)
      else do
        finNext <- TAC.findLastChar tac (y+1)
        if finNext<x
        then return (finNext+1,y+1)
        else return (x,y+1)

-- | handles arrows in Smart-mode
handleArrowsSpec :: String
  -> TAC.Position
  -> TAC.TextAreaContent
  -> IO(TAC.Position)
handleArrowsSpec key pos@(x,y) tac = do
  (maxX,maxY) <- TAC.size tac
  case key of
    "Left" -> return $
      if x==0 && y==0
      then (x,y)
      else
        if x==0
        then (maxX,y-1)
        else (x-1,y)
    "Right" -> return $
      if x==maxX && y==maxY
      then (x,y)
      else
        if x==maxX
        then (0,y+1)
        else (x+1,y)
    "Up" -> return $ if y==0 then (x,y) else (x,y-1)
    "Down" -> return $ if y==maxY then (x,y) else (x,y+1)

-- | handles Backspace-key
handleBackSpace :: TAC.TextAreaContent
  -> TAC.Position
  -> IO(TAC.Position)
handleBackSpace tac (x,y) = 
  case (x,y) of
    (0,0) -> return (0,0)
    (0,_) -> do
      finXPrev <- TAC.findLastChar tac (y-1)
      History.action tac (finXPrev+1,y-1) TAC.RemoveLine
      TACU.moveLinesUp tac y
      return(finXPrev+1,y-1)
    (_,_) -> do
      empty <- TAC.isEmptyLine tac y
      if empty
      then return(0,y)
      else do
        cell <- TAC.getCell tac (x-1,y)
        let (curchar, _) = if isNothing cell then (TAC.defaultChar, TAC.defaultColor) else fromJust cell
        History.action tac (x-1,y) (TAC.Remove [curchar])
        TAC.deleteCell tac (x-1,y)
        TACU.moveChars tac (x,y) (-1,0)
        return (x-1,y)

-- | handles Return-key in Smart-mode
handleReturnRail :: TAC.TextAreaContent
  -> TAC.Position
  -> IO(TAC.Position)
handleReturnRail tac pos@(x,y) = do
  History.action tac pos TAC.InsertLine
  TACU.moveLinesDownXShift tac pos False
  return (x,y+1)

-- | handles Return-key in Insert-mode and Replace-mode
handleReturn :: TAC.TextAreaContent
  -> TAC.Position
  -> IO(TAC.Position)
handleReturn tac pos@(x,y) = do
  History.action tac pos TAC.InsertLine
  TACU.moveLinesDownXShift tac pos True
  return (0,y+1)

-- | handles Tab-key and Shift-Tab
handleTab :: TAC.TextAreaContent
  -> TAC.Position
  -> [Modifier]
  -> IO(TAC.Position)
handleTab tac pos@(x,y) modif = do
  prevCharX <- TACU.findLastCharBefore tac (x-1) y
  finX <- TAC.findLastChar tac y
  case modif of
    [Shift] -> do
      if prevCharX == (-1)
      then
        if x>3
        then do
          History.action tac (x-4,y) (TAC.Remove "    ")
          TACU.moveChars tac pos (-4,0)
          return (x-4,y)
        else do
          History.action tac (0,y) (TAC.Remove (take x (repeat ' ')))
          TACU.moveChars tac pos (-x,0)
          return (0,y)
      else return pos
    _ -> do
      History.action tac pos (TAC.Insert "    ")
      TACU.moveChars tac pos (4,0)
      return(x+4,y)

-- | handles Delete-key
handleDelete :: TAC.TextAreaContent
  -> TAC.Position
  -> IO(TAC.Position)
handleDelete tac pos@(x,y) = do
  finX <- TAC.findLastChar tac y
  if x==finX+1
  then do
    History.action tac pos TAC.RemoveLine
    TACU.moveLinesUp tac (y+1)
    return (x,y)
  else do
    cell <- TAC.getCell tac pos
    let (curchar, _) = if isNothing cell then (TAC.defaultChar, TAC.defaultColor) else fromJust cell
    History.action tac pos (TAC.Remove [curchar])
    TAC.deleteCell tac (x,y)
    TACU.moveChars tac (x+1,y) (-1,0)
    return pos

-- Rail Smart-mode setting of Cursor-Position
-- directions
dNW :: TAC.Direction
dNW = (-1,-1)
dN :: TAC.Direction
dN = (0,-1)
dNO :: TAC.Direction
dNO = (1,-1)
dW :: TAC.Direction
dW = (-1,0)
dD :: TAC.Direction
dD = (0,0)
dO :: TAC.Direction
dO = (1,0)
dSW :: TAC.Direction
dSW = (-1,1)
dS :: TAC.Direction
dS = (0,1)
dSO :: TAC.Direction
dSO = (1,1)

-- | find new direction, sets (0,0) if input isundefined
getNewDirection :: Char -> TAC.Direction -> TAC.Direction
getNewDirection _ (0,0) = (1,0)
getNewDirection '*' x = x
getNewDirection '@' (x,y) = (-x,-y)
getNewDirection '+' dir
  |dir `elem` [dS, dN, dW, dO] = dir
  |otherwise = dD
getNewDirection 'x' dir
  | dir `elem` [dSO, dSW, dNO, dNW] = dir
  | otherwise = dD
getNewDirection '|' dir
  | dir `elem` [dS, dSW, dSO] = dS
  | dir `elem` [dN, dNO, dNW] = dN
  | otherwise = dD
getNewDirection '-' dir
  | dir `elem` [dO, dSO, dNO] = dO
  | dir `elem` [dW, dSW, dNW] = dW
  | otherwise = dD
getNewDirection '/' dir
  | dir `elem` [dO, dN, dNO] = dNO
  | dir `elem` [dW, dS, dSW] = dSW
  | otherwise = dD
getNewDirection '\\' dir
  | dir `elem` [dW, dN, dNW] = dNW
  | dir `elem` [dS, dO, dSO] = dSO
  | otherwise = dD
getNewDirection '<' dir
  | dir == dO  = dSO
  | dir == dSW = dW
  | dir == dNW = dNO
  | otherwise  = dD
getNewDirection '>' dir
  | dir == dW  = dNW
  | dir == dNO = dO
  | dir == dSO = dSW
  | otherwise  = dD
getNewDirection 'v' dir
  | dir == dN  = dNO
  | dir == dSO = dS
  | dir == dSW = dNW
  | otherwise  = dD
getNewDirection '^' dir
  | dir == dS  = dSW
  | dir == dNO = dSO
  | dir == dNW = dN
  | otherwise  = dD
