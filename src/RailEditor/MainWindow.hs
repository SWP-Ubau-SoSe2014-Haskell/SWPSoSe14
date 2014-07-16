{- |
Module      :  MainWindow.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

The MainWindow-module depicts the main window of the editor.
-}
module MainWindow (
  create
                  )
  where

    -- imports --
import qualified Graphics.UI.Gtk  as Gtk
import qualified MenuBar          as MB
import qualified ToolBar          as TB
import qualified FooterBar        as FB
import qualified TextArea         as TA
import TextAreaContent as TAC
import qualified InteractionField as IAF
import Data.IORef
import qualified Interpreter as IN
import qualified Paths_rail_compiler_editor as Path
import Control.Monad

    -- functions --

afterEvent evt ta footer = 
  evt (TA.drawingArea ta) $ \event -> do
    let posRef = TA.currentPosition ta
    readIORef posRef >>= FB.setPosition footer
    return True

-- | creates a mainWindow
create :: IO ()
create = do
  Gtk.initGUI
  -- create and configure main window
  window <- Gtk.windowNew
  iconpath <- Path.getDataFileName "data/icon.png"
  pb <- Gtk.pixbufNewFromFile iconpath
  Gtk.windowSetIcon window (Just pb)
  Gtk.onDestroy window Gtk.mainQuit

  interDT <- IAF.create
  let boxView = IAF.getContainer interDT
  footer <- FB.create
  let hboxInfoLine = FB.getContainer footer

  -- create TextArea with TextAreaContent
  tac <- TAC.init 100 100 (IAF.getInputBuffer interDT) (IAF.getOutputBuffer interDT)
  ta <- TA.initTextAreaWithContent tac
  lwin <- TA.getTextAreaContainer ta

  -- reset label with current position
  afterEvent Gtk.afterKeyPress ta footer

  afterEvent Gtk.afterButtonPress ta footer

  -- pack TextArea and InteractionField
  boxLay <- Gtk.hBoxNew False 0
  Gtk.boxPackStart boxLay lwin Gtk.PackGrow 1
  vSep <- Gtk.vSeparatorNew
  Gtk.boxPackStart boxLay vSep Gtk.PackNatural 2
  Gtk.boxPackEnd boxLay boxView Gtk.PackNatural 1

  table <- Gtk.tableNew 5 1 False
  -- avoid setting focus through key-events
  Gtk.containerSetFocusChain table [Gtk.toWidget $ TA.drawingArea ta]
  -- buffer for plug 'n' play
  let bufferOut = IAF.getOutputBuffer interDT
  let bufferIn  = IAF.getInputBuffer interDT

  Gtk.on bufferIn Gtk.bufferInsertText $ \iter string ->  do
    putStrLn "In"
    tac <- readIORef (TA.textAreaContent ta)
    cnt <- readIORef (TAC.context tac)
    let flags = TAC.railFlags cnt
    if TAC.Interpret `elem` flags
    then IN.interpret tac
    else when (TAC.Step `elem` flags) $ IN.step tac

  menuBar <- MB.create window ta bufferOut bufferIn
  extraBar <- TB.create ta footer interDT

  vSepa <- Gtk.hSeparatorNew

  -- fill table layout
  Gtk.tableAttach table menuBar 0 1 0 1 [Gtk.Fill] [Gtk.Fill] 0 0
  Gtk.tableAttach table extraBar 0 1 1 2 [Gtk.Fill] [Gtk.Fill] 0 0
  Gtk.tableAttach table boxLay 0 1 2 3 [Gtk.Expand,Gtk.Fill] [Gtk.Expand,Gtk.Fill] 0 0
  Gtk.tableAttach table vSepa 0 1 3 4 [Gtk.Fill] [Gtk.Fill] 0 0
  Gtk.tableAttach table hboxInfoLine 0 1 4 5 [Gtk.Fill] [Gtk.Fill] 2 2

  Gtk.set window [Gtk.containerChild Gtk.:= table,
              Gtk.windowDefaultHeight Gtk.:= 550,
              Gtk.windowDefaultWidth Gtk.:= 850,
              Gtk.windowWindowPosition Gtk.:= Gtk.WinPosCenter]
  Gtk.widgetShowAll window
  Gtk.mainGUI

