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
    keyToChar val == Just 'z')
  then
    if elem Shift modif
    then History.redo tac pos
    else History.undo tac pos
  else
    case modus of
      "Normal" -> handleKeyNorm tac pos modif key val
      "Insert" -> handleKeyIns tac pos modif key val
      "Special" -> handleKeySpec tac pos modif key val

handleKeyNorm :: TAC.TextAreaContent
  -> Position
  -> [Modifier]
  -> String
  -> KeyVal
  -> IO(TAC.Position)
handleKeyNorm tac pos@(x,y) modif key val = do
  putStrLn key
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
  let (curchar, _) = if isNothing cell then (' ', TAC.defaultColor) else fromJust cell
  History.action tac pos (TAC.Replace [curchar] [char])
  TAC.putCell tac pos (char,TAC.defaultColor)
  return (x+1,y)

handlePrintKeyNorm :: TAC.TextAreaContent -> TAC.Position -> String -> KeyVal -> IO(TAC.Position)
handlePrintKeyNorm tac pos@(x,y) key val = do
  finX <- TACU.findLastChar tac y
  let char = (if key=="dead_circumflex" then '^' else fromJust $ keyToChar val)
  History.action tac pos (TAC.Insert [char])
  TACU.moveChars tac x finX y (1,0)
  TAC.putCell tac (x,y) (char,TAC.defaultColor)
  return (x+1,y)

isArrow :: String
  -> Bool
isArrow key = elem key ["Left", "Right", "Up", "Down"]

--TODO adjust
handleArrowsNorm :: String
  -> TAC.Position
  -> TAC.TextAreaContent
  -> IO(TAC.Position)
handleArrowsNorm key pos@(x,y) tac = do
  (maxX,maxY) <- TAC.size tac
  case key of
    "Left" -> return $ if x==0 then (x,y) else (x-1,y)
    "Right" -> return $ if x==maxX then (x,y) else (x+1,y)
    "Up" -> return $ if y==0 then (x,y) else (x,y-1)
    "Down" -> return $ if y==maxY then (x,y) else (x,y+1)

handleArrowsSpec :: String
  -> TAC.Position
  -> TAC.TextAreaContent
  -> IO(TAC.Position)
handleArrowsSpec key pos@(x,y) tac = do
  (maxX,maxY) <- TAC.size tac
  case key of
    "Left" -> return $ if x==0 then (x,y) else (x-1,y)
    "Right" -> return $ if x==maxX then (x,y) else (x+1,y)
    "Up" -> return $ if y==0 then (x,y) else (x,y-1)
    "Down" -> return $ if y==maxY then (x,y) else (x,y+1)

handleBackSpace tac (x,y) = 
  case (x,y) of
    (0,0) -> return (0,0)
    (0,_) -> do
      finXPrev <- TACU.findLastChar tac (y-1)
      TACU.moveLinesUp tac y
      return(finXPrev+1,y-1)
    (_,_) -> do
      finX <- TACU.findLastChar tac y
      TAC.deleteCell tac (x-1,y)
      TACU.moveChars tac x finX y (-1,0)
      return (x-1,y)

handleReturnRail tac pos@(x,y) = do
  moveLinesDownXShift tac pos False
  return (x,y+1)

handleReturn tac pos@(x,y) = do
  moveLinesDownXShift tac pos True
  return (0,y+1)

handleTab tac pos@(x,y) modif = do
  prevCharX <- TACU.findLastCharBefore tac (x-1) y
  finX <- TACU.findLastChar tac y
  putStrLn $ show modif
  case modif of
    [Shift] -> do
      if prevCharX == (-1)
      then
        if x>3
        then do
          TACU.moveChars tac x finX y (-4,0)
          return (x-4,y)
        else do
          TACU.moveChars tac x finX y (-x,0)
          return (0,y)
      else return pos
    _ -> do
      TACU.moveChars tac x finX y (4,0)
      return(x+4,y)

handleDelete tac (x,y) = do
  finX <- TACU.findLastChar tac y
  if x==finX+1
  then do
    moveLinesUp tac (y+1)
    return (x,y)
  else do
    TAC.deleteCell tac (x,y)
    TACU.moveChars tac (x+1) (finX+1) y (-1,0)
    return(x,y)

