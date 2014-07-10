{- |
Module      :  ToolBar.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

The ToolBar-module depicts the tool bar at the top of the main window below the menu bar.
-}
module ToolBar (
  create
               )
  where
    
    -- imports --

import Data.IORef
import qualified KeyHandler as KH
import qualified FooterBar as FB
import qualified TextArea as TA
import qualified Graphics.UI.Gtk as Gtk
    -- functions --

-- | creates a toolbar
create area footer = do
    toolBar <- Gtk.menuBarNew

    -- create step button
    image <- Gtk.imageNewFromStock Gtk.stockMediaPlay Gtk.IconSizeMenu
    step <- Gtk.imageMenuItemNewWithLabel ""
    Gtk.imageMenuItemSetImage step image
    Gtk.menuShellAppend toolBar step

    -- create continue button
    image2 <- Gtk.imageNewFromStock Gtk.stockGotoLast Gtk.IconSizeMenu
    continue <- Gtk.imageMenuItemNewWithLabel ""
    Gtk.imageMenuItemSetImage continue image2
    Gtk.menuShellAppend toolBar continue

    -- create run button
    imageD <- Gtk.imageNewFromStock Gtk.stockGoForward Gtk.IconSizeMenu
    run <- Gtk.imageMenuItemNewWithLabel ""
    Gtk.imageMenuItemSetImage run imageD
    Gtk.menuShellAppend toolBar run

    -- create mode-menu
    mode <- Gtk.menuNew

    -- create modes
    insertMode <- Gtk.radioMenuItemNewWithLabel "insert"
    replaceMode <- Gtk.radioMenuItemNewWithLabelFromWidget insertMode "replace"
    smartMode <- Gtk.radioMenuItemNewWithLabelFromWidget insertMode "smart"

    highlightCheck <- Gtk.checkMenuItemNewWithLabel "highlighting"
    Gtk.checkMenuItemSetActive highlightCheck True

    -- set mode action
    Gtk.onButtonPress insertMode $ \event -> do
      TA.setInputMode area KH.Insert
      FB.setMode footer KH.Insert
      return True
    Gtk.onButtonPress replaceMode $ \event -> do
      TA.setInputMode area KH.Replace
      FB.setMode footer KH.Replace
      return True
    Gtk.onButtonPress smartMode $ \event -> do
      TA.setInputMode area KH.Smart
      FB.setMode footer KH.Smart
      return True

    Gtk.onButtonPress highlightCheck $ \event -> do
      isActive <- Gtk.checkMenuItemGetActive highlightCheck
      if isActive
      then TA.setHighlighting area False
      else TA.setHighlighting area True
      return True

    -- configure mode-menu
    modeItem <- Gtk.menuItemNewWithLabel "mode"
    Gtk.menuItemSetSubmenu modeItem mode

    Gtk.menuShellAppend toolBar modeItem

    Gtk.menuShellAppend mode insertMode
    Gtk.menuShellAppend mode replaceMode
    Gtk.menuShellAppend mode smartMode
    Gtk.menuShellAppend mode highlightCheck

    return toolBar
