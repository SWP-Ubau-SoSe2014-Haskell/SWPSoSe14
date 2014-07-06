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

    -- functions --
create :: IO ()
create = do
  Gtk.initGUI
  window <- Gtk.windowNew
  tac <- TAC.init 100 100
  ta <- TA.initTextAreaWithContent tac
  lwin <- TA.getTextAreaContainer ta

  boxView <- IAF.create
  hboxInfoLine <- FB.create

  boxLay <- Gtk.hBoxNew False 0
  Gtk.boxPackStart boxLay lwin Gtk.PackGrow 1
  vSep <- Gtk.vSeparatorNew
  Gtk.boxPackStart boxLay vSep Gtk.PackNatural 2
  Gtk.boxPackEnd boxLay boxView Gtk.PackNatural 1

  table <- Gtk.tableNew 5 1 False

  --JUST FOR TESTING
  bufferOut <- Gtk.textBufferNew Nothing

  menuBar <- MB.create window ta bufferOut
  extraBar <- TB.create

  vSepa <- Gtk.hSeparatorNew

  Gtk.tableAttach table menuBar 0 1 0 1 [Gtk.Fill] [Gtk.Fill] 0 0
  Gtk.tableAttach table extraBar 0 1 1 2 [Gtk.Fill] [Gtk.Fill] 0 0
  Gtk.tableAttach table boxLay 0 1 2 3 [Gtk.Expand,Gtk.Fill] [Gtk.Expand,Gtk.Fill] 0 0
  Gtk.tableAttach table vSepa 0 1 3 4 [Gtk.Fill] [Gtk.Fill] 0 0
  Gtk.tableAttach table hboxInfoLine 0 1 4 5 [Gtk.Fill] [Gtk.Fill] 2 2

  Gtk.set window [Gtk.containerChild Gtk.:= table,
              Gtk.windowDefaultHeight Gtk.:= 550,
              Gtk.windowDefaultWidth Gtk.:= 850,
              Gtk.windowWindowPosition Gtk.:= Gtk.WinPosCenter]

  Gtk.onDestroy window Gtk.mainQuit
  Gtk.widgetShowAll window
  Gtk.mainGUI
