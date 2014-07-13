{- |
Module      :  KeyHandler.hs
Description :  .
Maintainer  :  Kristin Knorr, Nicolas Lehmann, Benjamin Kodera (c)
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
import qualified Interpreter
import qualified Selection

data InputMode = Replace | Insert | Smart

-- | handleKey passes key depending on Entrymode and handles RedoUndo Shortcuts.
handleKey :: TAC.TextAreaContent
  -> TAC.Position
  -> InputMode
  -> [Modifier]
  -> String
  -> KeyVal
  -> IO TAC.Position
handleKey tac pos modus modif key val
  | elem Control modif && (keyToChar val == Just 'z' || keyToChar val == Just 'Z') =
    if Shift `elem` modif
    then History.redo tac pos
    else History.undo tac pos
  | Control `elem` modif && (keyToChar val == Just 'c' || keyToChar val == Just 'C') = do -- copy 
    TAC.setClipboard tac
    return pos
  | Control `elem` modif && (keyToChar val == Just 'v' || keyToChar val == Just 'V') = do -- paste
    clipboard <- TAC.getClipboard tac
    cells <- Selection.getCellsByPositons tac clipboard
    History.action tac pos (TAC.Insert cells)
    Selection.relocateCells tac clipboard pos
  | otherwise =
      if elem Control modif && keyToChar val == Just 'b'  --set breakpoint
      then Interpreter.toggleBreak tac pos >> return pos
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
  -> IO TAC.Position
handleKeyIns tac pos@(x,y) modif key val
  | isJust (keyToChar val) || key == "dead_circumflex" = handlePrintKeyIns tac pos key val
  | isArrow key = handleArrowsIns key pos tac
  | otherwise = case key of
    "BackSpace" -> handleBackSpace tac pos
    "Return" -> handleReturn tac pos
    "Tab" -> handleTab tac pos modif
    "ISO_Left_Tab" -> handleTab tac pos modif
    "Delete" -> handleDelete tac pos
    "Home" -> return (0, y)
    "End" -> do 
      finX <- TAC.findLastChar tac y
      return (if finX == (-1) then 0 else finX + 1, y)
    _ -> return pos

-- | handles keys in Replace-mode
handleKeyRP :: TAC.TextAreaContent
  -> TAC.Position
  -> [Modifier]
  -> String
  -> KeyVal
  -> IO TAC.Position
handleKeyRP tac pos@(x,y) modif key val
  | isJust (keyToChar val) || key == "dead_circumflex" = handlePrintKeyRP tac pos key val
  | isArrow key = handleArrowsIns key pos tac
  | otherwise = case key of
    "BackSpace" -> handleBackSpace tac pos
    "Return" -> handleReturn tac pos
    "Tab" -> handleTab tac pos modif
    "ISO_Left_Tab" -> handleTab tac pos modif
    "Delete" -> handleDelete tac pos
    "Home" -> return (0,y)
    "End" -> do
      finX <- TAC.findLastChar tac y
      return (if finX==(-1) then 0 else finX+1,y)
    _ -> return pos

-- | handles keys in Smart-mode
handleKeySpec :: TAC.TextAreaContent
  -> TAC.Position
  -> [Modifier]
  -> String
  -> KeyVal
  -> IO TAC.Position
handleKeySpec tac pos@(x,y) modif key val
  | isJust (keyToChar val) || key=="dead_circumflex" = handlePrintKeySpec tac pos key val
  | isArrow key = handleArrowsSpec key pos tac
  | otherwise = case key of
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
  -> IO TAC.Position
handlePrintKeyRP tac pos@(x,y) key val = do
  let char = if key=="dead_circumflex" then '^' else fromJust $ keyToChar val
  cell <- TAC.getCell tac pos
  let ((curchar,isSelected), _) = fromMaybe ((TAC.defaultChar, False), TAC.defaultColor) cell
  History.action tac pos (TAC.Replace [(curchar,False)] [(char,False)])
  TAC.putCell tac pos ((char,isSelected),TAC.defaultColor)
  return (x+1,y)

-- | handling of printable keys in Replace-mode and Smart-mode (overwriting)
handlePrintKeySpec :: TAC.TextAreaContent
  -> TAC.Position
  -> String
  -> KeyVal
  -> IO TAC.Position
handlePrintKeySpec tac pos@(x,y) key val = do
  let char = if key=="dead_circumflex" then '^' else fromJust $ keyToChar val
  cell <- TAC.getCell tac pos
  let (content@(curchar,isSelected), _) = fromMaybe ((TAC.defaultChar, False), TAC.defaultColor) cell
  History.action tac pos (TAC.Replace [content] [(char,False)])
  TAC.putCell tac pos ((char,False),TAC.defaultColor)
  return (x+1,y)

-- | handling of printable keys in Insert-mode
handlePrintKeyIns :: TAC.TextAreaContent
  -> TAC.Position
  -> String
  -> KeyVal
  -> IO TAC.Position
handlePrintKeyIns tac pos@(x,y) key val = do
  let char = if key=="dead_circumflex" then '^' else fromJust $ keyToChar val
  History.action tac pos (TAC.Insert [(char,False)])
  TACU.moveChars tac pos (1,0)
  TAC.putCell tac pos ((char,False),TAC.defaultColor)
  return (x+1,y)

-- | checking if key is an arrow 
isArrow :: String -> Bool
isArrow key = key `elem` ["Left", "Right", "Up", "Down"]

-- | handles arrows in Insert-mode and Replace-mode
handleArrowsIns :: String
  -> TAC.Position
  -> TAC.TextAreaContent
  -> IO TAC.Position
handleArrowsIns key pos@(x,y) tac = do
  (maxX,maxY) <- TAC.size tac
  case key of
    "Left" 
      | x==0 && y==0 -> return (x,y)
      | x==0 -> do
        finPrev <- TAC.findLastChar tac (y-1)
        return (finPrev+1,y-1)
      | otherwise -> return (x-1,y)
    "Right" ->
      return $ if x==maxX && y==maxY then (x,y) else 
        if x==maxX then (0,y+1) else (x+1,y)
    "Up" ->
      if y==0
      then return (x,y)
      else do
        finPrev <- TAC.findLastChar tac (y-1)
        return $ if finPrev<x then (finPrev+1,y-1) else (x,y-1)
    "Down" ->
      if y==maxY
      then return (x,y)
      else do
        finNext <- TAC.findLastChar tac (y+1)
        return $ if finNext<x then (finNext+1,y+1) else (x,y+1)

-- | handles arrows in Smart-mode
handleArrowsSpec :: String
  -> TAC.Position
  -> TAC.TextAreaContent
  -> IO TAC.Position
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
  -> IO TAC.Position 
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
        maybeCell <- TAC.getCell tac (x-1,y)
        let cell = fromMaybe ((TAC.defaultChar, False), TAC.defaultColor) maybeCell
        History.action tac (x-1,y) (TAC.Remove [fst cell])
        TAC.deleteCell tac (x-1,y)
        TACU.moveChars tac (x,y) (-1,0)
        return (x-1,y)

-- | handles Return-key in Smart-mode
handleReturnRail :: TAC.TextAreaContent
  -> TAC.Position
  -> IO TAC.Position
handleReturnRail tac pos@(x,y) = do
  History.action tac pos TAC.InsertLine
  TACU.moveLinesDownXShift tac pos False
  return (x,y+1)

-- | handles Return-key in Insert-mode and Replace-mode
handleReturn :: TAC.TextAreaContent
  -> TAC.Position
  -> IO TAC.Position
handleReturn tac pos@(x,y) = do
  History.action tac pos TAC.InsertLine
  TACU.moveLinesDownXShift tac pos True
  return (0,y+1)

-- | handles Tab-key and Shift-Tab
handleTab :: TAC.TextAreaContent
  -> TAC.Position
  -> [Modifier]
  -> IO TAC.Position
handleTab tac pos@(x,y) modif = do
  prevCharX <- TACU.findLastCharBefore tac (x-1) y
  finX <- TAC.findLastChar tac y
  case modif of
    [Shift] ->
      if prevCharX == (-1)
      then
        if x>3
        then do
          History.action tac (x-4,y) (TAC.Remove (replicate 4 (' ', False)))
          TACU.moveChars tac pos (-4,0)
          return (x-4,y)
        else do
          History.action tac (0,y) (TAC.Remove (replicate x (' ', False)))
          TACU.moveChars tac pos (-x,0)
          return (0,y)
      else return pos
    _ -> do
      History.action tac pos (TAC.Insert (replicate 4 (' ', False)))
      TACU.moveChars tac pos (4,0)
      return(x+4,y)

-- | handles Delete-key
handleDelete :: TAC.TextAreaContent
  -> TAC.Position
  -> IO TAC.Position
handleDelete tac pos@(x,y) = do
  finX <- TAC.findLastChar tac y
  if x==finX+1
  then do
    History.action tac pos TAC.RemoveLine
    TACU.moveLinesUp tac (y+1)
    return (x,y)
  else do
    maybeCell <- TAC.getCell tac pos
    let cell = fromMaybe ((TAC.defaultChar, False), TAC.defaultColor) maybeCell
    History.action tac pos (TAC.Remove [fst cell])
    TAC.deleteCell tac (x,y)
    TACU.moveChars tac (x+1,y) (-1,0)
    return pos

