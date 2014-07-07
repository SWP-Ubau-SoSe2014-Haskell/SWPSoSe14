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

    -- functions --
    invert :: (TAC.Action, TAC.Position) -> (TAC.Action, TAC.Position)
    invert (TAC.Concat act1 act2, pos) = (TAC.Concat (invert act1) (invert act2), pos)
    invert (TAC.Remove string, pos) = (TAC.Insert string, pos)
    invert (TAC.Insert string, pos) = (TAC.Remove string, pos)
    invert (TAC.Replace a b, pos) = (TAC.Replace b a, pos)
    invert (TAC.MoveTo a, b) = (TAC.MoveTo b, a)

    -- add a given function to our queues
    action :: TAC.TextAreaContent -> TAC.Position -> TAC.Action -> IO ()
    action tac position useraction = do
      redoqueue <- readIORef (TAC.redoQueue tac)
      when (not (null redoqueue)) $ writeIORef (TAC.redoQueue tac) []
      undoqueue <- readIORef (TAC.undoQueue tac)
      writeIORef (TAC.undoQueue tac) (invert (useraction, position):undoqueue)

    -- gets the action to to and move it from one queue to the opposite one
    shiftaction :: (TAC.ActionQueue, TAC.ActionQueue) -> (TAC.ActionQueue, TAC.ActionQueue, (TAC.Action, TAC.Position))
    shiftaction (from, to) = (tail from, invert (head from):to, head from)

    -- run whatever action given
    runaction :: TAC.TextAreaContent -> TAC.Position -> (TAC.Action, TAC.Position) -> IO (TAC.Position)
    runaction tac pos (TAC.Concat act1 act2, actpos) = return pos
    runaction tac pos (TAC.Remove string, actpos) = do
      TAC.deleteCell tac actpos
      return actpos
    runaction tac pos (TAC.Insert [], actpos) = return actpos
    runaction tac pos (TAC.Insert (x:xs), actpos) = do
      TAC.putCell tac actpos (x, TAC.defaultColor)
      runaction tac pos (TAC.Insert xs, actpos)
    runaction tac pos (TAC.Replace a [], actpos) = return pos
    runaction tac pos (TAC.Replace a (x:xs), actpos) = do
      TAC.putCell tac actpos (x, TAC.defaultColor)
      runaction tac pos (TAC.Replace a xs, actpos)
    runaction tac pos (TAC.MoveTo a, b) = return pos

    -- allows to undo actions in the editor
    undo :: TAC.TextAreaContent -> TAC.Position -> IO (TAC.Position)
    undo tac pos = do
      undoqueue <- readIORef (TAC.undoQueue tac)
      if (not (null undoqueue))
      then do
        redoqueue <- readIORef (TAC.redoQueue tac)
        undoqueue <- readIORef (TAC.undoQueue tac)
        let (newundo, newredo, action) = shiftaction (undoqueue, redoqueue)
        writeIORef (TAC.redoQueue tac) newredo
        writeIORef (TAC.undoQueue tac) newundo
        runaction tac pos action
      else
        return pos

    -- allows to redo actions in the editor
    redo :: TAC.TextAreaContent -> TAC.Position -> IO (TAC.Position)
    redo tac pos = do
      redoqueue <- readIORef (TAC.redoQueue tac)
      if (not (null redoqueue))
      then do
        redoqueue <- readIORef (TAC.redoQueue tac)
        undoqueue <- readIORef (TAC.undoQueue tac)
        let (newredo, newundo, action) = shiftaction (redoqueue, undoqueue)
        writeIORef (TAC.redoQueue tac) newredo
        writeIORef (TAC.undoQueue tac) newundo
        runaction tac pos action
      else
        return pos
