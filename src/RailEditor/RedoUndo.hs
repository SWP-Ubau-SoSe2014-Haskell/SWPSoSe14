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
    import qualified TextAreaContent as TAC
    import qualified TextAreaContentUtils as TACU

    -- functions --
    invert :: (TAC.Action, TAC.Position) -> (TAC.Action, TAC.Position)
    invert (TAC.Remove string, pos) = (TAC.Insert string, pos)
    invert (TAC.Insert string, pos) = (TAC.Remove string, pos)
    invert (TAC.Replace a b, pos) = (TAC.Replace b a, pos)
    invert (TAC.MoveTo a, b) = (TAC.MoveTo b, a)

    -- add a given function to our queues
    addaction :: TAC.TextAreaContent -> TAC.Action -> TAC.Position -> IO ()
    addaction tac action position
     | not (null redoqueue) = do
        writeIORef (TAC.redoQueue tac) []
        addaction tac action position
     | otherwise = do
        undoqueue <- readIORef (TAC.undoQueue tac)
        writeIORef (TAC.undoQueue tac) (invert (action, position):undoqueue)

    -- gets the action to to and move it from one queue to the opposite one
    shiftaction :: (TAC.ActionQueue, TAC.ActionQueue) -> (TAC.ActionQueue, TAC.ActionQueue, TAC.Action)
    shiftaction (from, to) = (tail from, invert (head from):to, head from)

    -- convert something from the keyhandler to an action
    toaction :: ??? -> TAC.Action
    toaction = undefined

    -- run whatever action given
    runaction :: TAC.Action -> IO ()
    runaction = undefined

    -- something was done in the editor
    action :: TAC.TextAreaContent -> ??? -> IO ()
    action tac keypress = addaction tac (toaction keypress)

    -- allows to undo actions in the editor
    undo :: TAC.TextAreaContent -> IO ()
    undo tac
      | null (TAC.undoQueue tac) = return ()
      | otherwise = do
         redoqueue <- readIORef (TAC.redoQueue tac)
         undoqueue <- readIORef (TAC.undoQueue tac)
         let (newundo, newredo, action) = shiftaction (undoqueue, redoqueue)
         writeIORef (TAC.redoQueue tac) newredo
         writeIORef (TAC.undoQueue tac) newundo
         runaction action

    -- allows to redo actions in the editor
    redo :: TAC.TextAreaContent -> IO ()
    redo tac
      | null (TAC.redoQueue tac) = return ()
      | otherwise = do
         redoqueue <- readIORef (TAC.redoQueue tac)
         undoqueue <- readIORef (TAC.undoQueue tac)
         let (newredo, newundo, action) = shiftaction (redoqueue, undoqueue)
         writeIORef (TAC.redoQueue tac) newredo
         writeIORef (TAC.undoQueue tac) newundo
         runaction action
