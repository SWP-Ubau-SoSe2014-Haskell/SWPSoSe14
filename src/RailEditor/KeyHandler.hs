{- |
Module      :  KeyHandler.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

The KeyHandler-module allows to react on keypress-events in the editor.
-}
module KeyHandler (
                   handleKey,   -- handles keypress-events
                  )
  where

import Graphics.UI.Gtk
import Control.Monad
import Data.Maybe
import TextAreaContent as TAC
import TextAreaContentUtils as TACU
import qualified RedoUndo as History

--handleKey tac pos modus modif key val
handleKey :: TAC.TextAreaContent
  -> Position
  -> String
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
      "Insert" -> handleKeyNorm tac pos modif key val
      "Replace" -> handleKeyIns tac pos modif key val
      "Smart" -> handleKeySpec tac pos modif key val

handleKeyNorm :: TAC.TextAreaContent
  -> Position
  -> [Modifier]
  -> String
  -> KeyVal
  -> IO(TAC.Position)
handleKeyNorm tac pos@(x,y) modif key val = do
  --putStrLn key
  if ((isJust $ keyToChar val) || key=="dead_circumflex")
  then handlePrintKeyNorm tac pos key val
  else do
    if isArrow key
    then handleArrowsNorm key pos tac
    else
      case key of
        "BackSpace" -> handleBackSpace tac pos
        "Return" -> handleReturn tac pos
        "Tab" -> handleTab tac pos modif
        "Delete" -> handleDelete tac pos
        "Home" -> return (0,y)
        "End" -> do
          finX <- TACU.findLastChar tac y
          return ((if finX==(-1) then 0 else finX+1),y)
        _ -> return pos

handleKeyIns :: TAC.TextAreaContent
  -> Position
  -> [Modifier]
  -> String
  -> KeyVal
  -> IO(TAC.Position)
handleKeyIns tac pos@(x,y) modif key val =
  if ((isJust $ keyToChar val) || key=="dead_circumflex")
  then handlePrintKeyIns tac pos key val
  else
    if isArrow key
    then handleArrowsNorm key pos tac
    else
      case key of
        "BackSpace" -> handleBackSpace tac pos
        "Return" -> handleReturn tac pos
        "Tab" -> handleTab tac pos modif
        "Delete" -> handleDelete tac pos
        "Home" -> return (0,y)
        "End" -> do
          finX <- TACU.findLastChar tac y
          return ((if finX==(-1) then 0 else finX+1),y)
        _ -> return pos

handleKeySpec :: TAC.TextAreaContent
  -> Position
  -> [Modifier]
  -> String
  -> KeyVal
  -> IO(TAC.Position)
handleKeySpec tac pos@(x,y) modif key val =
  if ((isJust $ keyToChar val) || key=="dead_circumflex")
  then handlePrintKeyIns tac pos key val
  else
    if isArrow key
    then
      handleArrowsSpec key pos tac
    else
      case key of
        "BackSpace" -> handleBackSpace tac pos
        "ReturnSpec" -> handleReturnRail tac pos
        "Tab" -> handleTab tac pos modif
        "Delete" -> handleDelete tac pos
        "Home" -> return (0,y)
        "End" -> do
          finX <- TACU.findLastChar tac y
          return (finX+1,y)
        _ -> return pos

handlePrintKeyIns :: TAC.TextAreaContent -> TAC.Position -> String -> KeyVal -> IO(TAC.Position)
handlePrintKeyIns tac pos@(x,y) key val = do
  let char = (if key=="dead_circumflex" then '^' else fromJust $ keyToChar val)
  cell <- TAC.getCell tac pos
  let (curchar, _) = if isNothing cell then (TAC.defaultChar, TAC.defaultColor) else fromJust cell
  History.action tac pos (TAC.Replace [curchar] [char])
  TAC.putCell tac pos (char,TAC.defaultColor)
  return (x+1,y)

handlePrintKeyNorm :: TAC.TextAreaContent -> TAC.Position -> String -> KeyVal -> IO(TAC.Position)
handlePrintKeyNorm tac pos@(x,y) key val = do
  let char = (if key=="dead_circumflex" then '^' else fromJust $ keyToChar val)
  History.action tac pos (TAC.Insert [char])
  TACU.moveChars tac pos (1,0)
  TAC.putCell tac pos (char,TAC.defaultColor)
  return (x+1,y)

isArrow :: String
  -> Bool
isArrow key = elem key ["Left", "Right", "Up", "Down"]

handleArrowsNorm :: String
  -> TAC.Position
  -> TAC.TextAreaContent
  -> IO(TAC.Position)
handleArrowsNorm key pos@(x,y) tac = do
  (maxX,maxY) <- TAC.size tac
  case key of
    "Left" ->
      if x==0 && y==0
      then return (x,y)
      else
        if x==0
        then do
          finPrev <- TACU.findLastChar tac (y-1)
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
        finPrev <- TACU.findLastChar tac (y-1)
        if finPrev<x
        then return (finPrev+1,y-1)
        else return (x,y-1)
    "Down" ->
      if y==maxY
      then return (x,y)
      else do
        finNext <- TACU.findLastChar tac (y+1)
        if finNext<x
        then return (finNext+1,y+1)
        else return (x,y+1)

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

handleBackSpace tac (x,y) = 
  case (x,y) of
    (0,0) -> return (0,0)
    (0,_) -> do
      finXPrev <- TACU.findLastChar tac (y-1)
      History.action tac (finXPrev+1,y-1) (TAC.Remove "\n")
      TACU.moveLinesUp tac y
      return(finXPrev+1,y-1)
    (_,_) -> do
      finX <- TACU.findLastChar tac y
      if finX==(-1)
      then return(0,y)
      else do
        cell <- TAC.getCell tac (x-1,y)
        let (curchar, _) = if isNothing cell then (TAC.defaultChar, TAC.defaultColor) else fromJust cell
        History.action tac (x-1,y) (TAC.Remove [curchar])
        TAC.deleteCell tac (x-1,y)
        TACU.moveChars tac (x,y) (-1,0)
        return (x-1,y)

handleReturnRail tac pos@(x,y) = do
  History.action tac pos (TAC.Insert ('\n':(take x (repeat ' '))))
  moveLinesDownXShift tac pos False
  return (x,y+1)

handleReturn tac pos@(x,y) = do
  History.action tac pos (TAC.Insert "\n")
  moveLinesDownXShift tac pos True
  return (0,y+1)

handleTab tac pos@(x,y) modif = do
  prevCharX <- TACU.findLastCharBefore tac (x-1) y
  finX <- TACU.findLastChar tac y
  --putStrLn $ show modif
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

handleDelete tac pos@(x,y) = do
  finX <- TACU.findLastChar tac y
  if x==finX+1
  then do
    History.action tac pos (TAC.Remove "\n")
    moveLinesUp tac (y+1)
    return (x,y)
  else do
    cell <- TAC.getCell tac pos
    let (curchar, _) = if isNothing cell then (TAC.defaultChar, TAC.defaultColor) else fromJust cell
    History.action tac pos (TAC.Remove [curchar])
    TAC.deleteCell tac (x,y)
    TACU.moveChars tac (x+1,y) (-1,0)
    return pos

