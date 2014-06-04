module Main where

import Graphics.UI.Gtk
import Data.Map as Map
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.Maybe
import Menu
import TextArea


main :: IO()
main = do
    initGUI
    window <- windowNew
    windowSetDefaultSize window 840 550
    windowSetPosition window WinPosCenter
    layout <- layoutNew Nothing Nothing
    menuBar <- createMenu window
    table <- tableNew 0 0 False
    tableAttach table menuBar 0 1 0 1 [] [] 0 0
    layoutPut layout table 0 0
    textArea <- textAreaNew layout 10 10
    set window [containerChild := layout ]
    changeColorOfCurrentEntry textArea (Color 65000 0 0)
    changeColorOfEntryByCoord textArea (2,2) (Color 65000 0 0)
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
    return ()

