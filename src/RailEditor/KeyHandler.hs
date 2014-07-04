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


handleKey :: TAC.TextAreaContent -> Position -> String -> KeyVal -> IO(TAC.Position)
handleKey tac (x,y) key val = 
  if isJust $ keyToChar val
  then do
    putStrLn "1"
    let char = fromJust $ keyToChar val
    putStrLn "2"
    handlePrintKeyEinfg tac (x,y) char
    putStrLn "3"
    return (((x+1),y))
  else return (5,6)

handlePrintKeyEinfg :: TAC.TextAreaContent -> TAC.Position -> Char -> IO()
handlePrintKeyEinfg tac pos val = do
  putCell tac pos (val,TAC.defaultColor)
--handlePrintKeyNorm

--handleBackspace

--handleReturnRail

--handleReturn

--handleTab

--handleShiftTab

--handleDelete

--handlePos1

--handleEnd
