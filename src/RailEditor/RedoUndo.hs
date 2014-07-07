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
      --putStrLn (show undoqueue)
      writeIORef (TAC.undoQueue tac) (invert (useraction, position):undoqueue)

    -- gets the action to to and move it from one queue to the opposite one
    shiftaction :: (TAC.ActionQueue, TAC.ActionQueue) -> (TAC.ActionQueue, TAC.ActionQueue, (TAC.Action, TAC.Position))
    shiftaction (from, to) = (tail from, invert (head from):to, head from)

    -- run whatever action given
    runaction :: TAC.TextAreaContent -> (TAC.Action, TAC.Position) -> IO ()
    runaction tac (TAC.Concat act1 act2, pos) = undefined
    runaction tac (TAC.Remove string, pos) = undefined
    runaction tac (TAC.Insert string, pos) = undefined
    runaction tac (TAC.Replace a [], pos) = return ()
    runaction tac (TAC.Replace a (x:xs), pos) = do
      TAC.putCell tac pos (x, TAC.defaultColor)
      runaction tac (TAC.Replace a xs, pos)
    runaction tac (TAC.MoveTo a, b) = undefined

    -- allows to undo actions in the editor
    undo :: TAC.TextAreaContent -> IO ()
    undo tac = do
      undoqueue <- readIORef (TAC.undoQueue tac)
      when (not (null undoqueue)) $ do
        redoqueue <- readIORef (TAC.redoQueue tac)
        undoqueue <- readIORef (TAC.undoQueue tac)
        let (newundo, newredo, action) = shiftaction (undoqueue, redoqueue)
        writeIORef (TAC.redoQueue tac) newredo
        writeIORef (TAC.undoQueue tac) newundo
        runaction tac action

    -- allows to redo actions in the editor
    redo :: TAC.TextAreaContent -> IO ()
    redo tac = do
      redoqueue <- readIORef (TAC.redoQueue tac)
      when (not (null redoqueue)) $ do
        redoqueue <- readIORef (TAC.redoQueue tac)
        undoqueue <- readIORef (TAC.undoQueue tac)
        let (newredo, newundo, action) = shiftaction (redoqueue, undoqueue)
        writeIORef (TAC.redoQueue tac) newredo
        writeIORef (TAC.undoQueue tac) newundo
        runaction tac action
