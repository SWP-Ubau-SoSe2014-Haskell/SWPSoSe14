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

import qualified Graphics.UI.Gtk as Gtk
    -- functions --

create = do
    toolBar <- Gtk.menuBarNew

    image <- Gtk.imageNewFromStock Gtk.stockExecute Gtk.IconSizeMenu
    run <- Gtk.imageMenuItemNewWithLabel ""
    Gtk.imageMenuItemSetImage run image
    Gtk.menuShellAppend toolBar run
    imageD <- Gtk.imageNewFromStock Gtk.stockGoForward Gtk.IconSizeMenu

    debugg <- Gtk.imageMenuItemNewWithLabel ""
    Gtk.imageMenuItemSetImage debugg imageD
    Gtk.menuShellAppend toolBar debugg

    mode <- Gtk.menuNew
    replaceMode <- Gtk.radioMenuItemNewWithLabel "replace"
    insertMode <- Gtk.radioMenuItemNewWithLabelFromWidget replaceMode "insert"
    smartMode <- Gtk.radioMenuItemNewWithLabelFromWidget replaceMode "smart"

    modeItem <- Gtk.menuItemNewWithLabel "mode"
    Gtk.menuItemSetSubmenu modeItem mode

    Gtk.menuShellAppend toolBar modeItem

    Gtk.menuShellAppend mode replaceMode
    Gtk.menuShellAppend mode insertMode
    Gtk.menuShellAppend mode smartMode

    return toolBar
