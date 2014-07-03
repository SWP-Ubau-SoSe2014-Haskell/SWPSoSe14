{- |
Module      :  MainWindow.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

The MainWindow-module depicts the main window of the editor.
-}
module MainWindow (
  createMainWindow
                  )
  where
    
    -- imports --
import Graphics.UI.Gtk
import qualified MenuBar          as MB
import qualified ToolBar          as TB
import qualified FooterBar        as FB
import qualified TextArea         as TA
import TextAreaContent as TAC
import qualified InteractionField as IAF

    -- functions --
createMainWindow :: IO ()
createMainWindow = do
  initGUI
  window <- windowNew
  tac <- TAC.init 100 100
  ta <- TA.initTextAreaWithContent tac
  f <- TA.getTextAreaContainer ta
  window `containerAdd` f
  set window [windowDefaultHeight := 550,
    windowDefaultWidth := 850, windowWindowPosition := WinPosCenter]
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
  return () 
