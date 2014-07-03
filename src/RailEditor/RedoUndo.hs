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

-- this has to be part of TAC
-- TAC { ...  undoQueue: IORef ActionQueue, redoQueue: IORef ActionQueue }
    data Action = Remove String | Insert String | Replace String String | MoveTo Position
		type ActionQueue = [(Action, Position)]

    -- functions --
    invert :: (Action, Position) -> (Action, Position)
    invert (Remove string, pos) = (Insert string, pos)
    invert (Insert string, pos) = (Remove string, pos)
    invert (Replace a b, pos) = (Replace b a, pos)
    invert (MoveTo a, b) = (MoveTo b, a)

    -- add a given function to our queues
    addaction :: TextAreaContent -> Action -> Position -> IO ()
    addaction tac action position
     | not (null redoqueue) = do
        writeIORef (redoQueue tac) []
        addaction tac action position
     | otherwise = do
        undoqueue <- readIORef (undoQueue tac)
        writeIORef (undoQueue tac) (invert (action, position):undoqueue)

    -- gets the action to to and move it from one queue to the opposite one
    shiftaction :: (ActionQueue, ActionQueue) -> (ActionQueue, ActionQueue, Action)
    shiftaction (from, to) = (tail from, invert (head from):to, head from)

    -- convert something from the keyhandler to an action
    toaction :: KeyPress -> Action
    toaction = undefined

    -- run whatever action given
    runaction :: Action -> IO ()
    runaction = undefined

    -- something was done in the editor
    action :: TextAreaContent -> KeyPress -> IO ()
    action tac keypress = addaction tac (toaction keypress)

    -- allows to undo actions in the editor
    undo :: TextAreaContent -> IO ()
    undo tac
      | null (undoQueue tac) = return ()
      | otherwise = do
         redoqueue <- readIORef (redoQueue tac)
         undoqueue <- readIORef (undoQueue tac)
         let (newundo, newredo, action) = shiftaction (undoqueue, redoqueue)
				 writeIORef (redoQueue tac) newredo
         writeIORef (undoQueue tac) newundo
         runaction action

    -- allows to redo actions in the editor
    redo :: TextAreaContent -> IO ()
    redo tac
      | null (redoQueue tac) = return ()
      | otherwise = do
         redoqueue <- readIORef (redoQueue tac)
         undoqueue <- readIORef (undoQueue tac)
         let (newredo, newundo, action) = shiftaction (redoqueue, undoqueue)
				 writeIORef (redoQueue tac) newredo
         writeIORef (undoQueue tac) newundo
         runaction action
