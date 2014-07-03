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
import TextAreaContent as TAC


handleKey :: TAC.TextAreaContent -> Position -> String -> IO(TAC.Position)
handleKey textAreaContent pos key = do
  return (0,0)
 
