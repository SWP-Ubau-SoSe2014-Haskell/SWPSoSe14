{- |
Module      :  RedoUndo.hs
Description :  .
Maintainer  :  Christian H. et al.
License     :  MIT

Stability   :  experimental

The RedoUndo-module contains three functions that allow to redo- and undo-actions in the editor.
-}
module RedoUndo (
                 action,-- something was done in the editor
                 undo,  -- allows to undo actions in the editor
                 redo   -- allows to redo actions in the editor
                )
  where
    
    -- imports --
    import Data.IORef
    import Control.Monad
    import qualified TextAreaContent as TAC
    import qualified TextAreaContentUtils as TACU
    
    data UndoRedo = Undo | Redo deriving Eq

    -- functions --
    invert :: (TAC.Action, TAC.Position) -> (TAC.Action, TAC.Position)
    invert a@(TAC.DoNothing, pos) = a
    invert (TAC.Concat act1 act2, pos) = (TAC.Concat (invert act1) (invert act2), pos)
    invert (TAC.Remove content, pos) = (TAC.Insert content, pos)
    invert (TAC.Insert content, pos) = (TAC.Remove content, pos)
    invert (TAC.Replace a b, pos) = (TAC.Replace b a, pos)
    invert (TAC.RemoveLine, (x, y)) = (TAC.InsertLine, (x, y-1))
    invert (TAC.InsertLine, (x, y)) = (TAC.RemoveLine, (x, y+1))

    -- add a given function to our queues
    action :: TAC.TextAreaContent -> TAC.Position -> TAC.Action -> IO ()
    action tac position useraction = do
      redoqueue <- readIORef (TAC.redoQueue tac)
      unless (null redoqueue) $ writeIORef (TAC.redoQueue tac) []
      undoqueue <- readIORef (TAC.undoQueue tac)
      writeIORef (TAC.undoQueue tac) (invert (useraction, position):undoqueue)

    -- gets the action to to and move it from one queue to the opposite one
    shiftaction :: (TAC.ActionQueue, TAC.ActionQueue) -> (TAC.ActionQueue, TAC.ActionQueue, (TAC.Action, TAC.Position))
    shiftaction (from, to) = (tail from, invert (head from):to, head from)

    -- run whatever action given
    runaction :: TAC.TextAreaContent -> (TAC.Action, TAC.Position) -> IO TAC.Position
    runaction _ (TAC.DoNothing, actpos) = return actpos
    runaction tac (TAC.Concat act1 act2, actpos) =
      runaction tac act1 >> runaction tac act2
    runaction tac (TAC.Remove [], actpos) = return actpos
    runaction tac (TAC.Remove (x:xs), actpos) = do
      TAC.deleteCell tac actpos
      TACU.moveChars tac actpos (-1,0)
      runaction tac (TAC.Remove xs, actpos)
    runaction tac (TAC.Insert [], actpos) = return actpos
    runaction tac (TAC.Insert (content@(char,_):xs), actpos) = do
      if char == '\n'
      then TACU.moveLinesDownXShift tac actpos True
      else do
        TACU.moveChars tac actpos (1,0)
        TAC.putCell tac actpos (content, TAC.defaultColor)
      runaction tac (TAC.Insert xs, actpos)
    runaction tac (TAC.Replace a [], actpos) = return actpos
    runaction tac (TAC.Replace a (content@(char,_):xs), actpos@(px, py)) =
      if char == ' '
      then runaction tac (TAC.Remove [content], actpos)
      else do
        TAC.putCell tac actpos (content, TAC.defaultColor)
        runaction tac (TAC.Replace a xs, (px+1, py))
    runaction tac (TAC.RemoveLine, (x, y)) = do
      TACU.moveLinesUp tac y
      return (x, y-1)
    runaction tac (TAC.InsertLine, (x, y)) = do
      TACU.moveLinesDownXShift tac (x, y) True
      return (0, y+1)

    -- allows to undo actions in the editor
    undo :: TAC.TextAreaContent -> TAC.Position -> IO TAC.Position
    undo tac pos = do
      undoqueue <- readIORef (TAC.undoQueue tac)
      if not (null undoqueue)
      then runRedoUndo tac Undo
      else return pos

    -- allows to redo actions in the editor
    redo :: TAC.TextAreaContent -> TAC.Position -> IO TAC.Position
    redo tac pos = do
      redoqueue <- readIORef (TAC.redoQueue tac)
      if not (null redoqueue)
      then runRedoUndo tac Redo
      else return pos

    runRedoUndo :: TAC.TextAreaContent -> UndoRedo -> IO TAC.Position
    runRedoUndo tac cmd = do
      redoqueue <- readIORef (TAC.redoQueue tac)
      undoqueue <- readIORef (TAC.undoQueue tac)
      if cmd == Undo then do
        let (newundo, newredo, action) = shiftaction (undoqueue, redoqueue)
        exec tac newundo newredo action
      else do
        let (newredo, newundo, action) = shiftaction (redoqueue, undoqueue) 
        exec tac newundo newredo action
     
    exec :: TAC.TextAreaContent -> TAC.ActionQueue -> TAC.ActionQueue -> (TAC.Action,TAC.Position) -> IO TAC.Position
    exec tac u r action = do
      writeIORef (TAC.redoQueue tac) r
      writeIORef (TAC.undoQueue tac) u
      runaction tac action
        
