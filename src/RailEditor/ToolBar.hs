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

import qualified KeyHandler as KH
import qualified TextArea as TA
import qualified Graphics.UI.Gtk as Gtk
    -- functions --

create area = do
    toolBar <- Gtk.menuBarNew

    image <- Gtk.imageNewFromStock Gtk.stockMediaPlay Gtk.IconSizeMenu
    step <- Gtk.imageMenuItemNewWithLabel ""
    Gtk.imageMenuItemSetImage step image
    Gtk.menuShellAppend toolBar step

    image2 <- Gtk.imageNewFromStock Gtk.stockGotoLast Gtk.IconSizeMenu
    continue <- Gtk.imageMenuItemNewWithLabel ""
    Gtk.imageMenuItemSetImage continue image2
    Gtk.menuShellAppend toolBar continue

    imageD <- Gtk.imageNewFromStock Gtk.stockGoForward Gtk.IconSizeMenu

    run <- Gtk.imageMenuItemNewWithLabel ""
    Gtk.imageMenuItemSetImage run imageD
    Gtk.menuShellAppend toolBar run

    mode <- Gtk.menuNew
    insertMode <- Gtk.radioMenuItemNewWithLabel "insert"
    replaceMode <- Gtk.radioMenuItemNewWithLabelFromWidget insertMode "replace"
    smartMode <- Gtk.radioMenuItemNewWithLabelFromWidget insertMode "smart"

    Gtk.onButtonPress insertMode $ \event -> TA.setInputMode area KH.Insert >> return True
    Gtk.onButtonPress replaceMode $ \event -> TA.setInputMode area KH.Replace >> return True
    Gtk.onButtonPress smartMode $ \event -> TA.setInputMode area KH.Smart >> return True

    modeItem <- Gtk.menuItemNewWithLabel "mode"
    Gtk.menuItemSetSubmenu modeItem mode

    Gtk.menuShellAppend toolBar modeItem

    Gtk.menuShellAppend mode insertMode
    Gtk.menuShellAppend mode replaceMode
    Gtk.menuShellAppend mode smartMode

    return toolBar
