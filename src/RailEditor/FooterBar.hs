{- |
Module      :  FooterBar.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

The FooterBar-module depicts the footer bar at the bottom of the main-window.
-}
module FooterBar (
  create
                 )
  where
    
    -- imports --
import qualified Graphics.UI.Gtk as Gtk
    -- functions --

create = do
  hboxInfoLine <- Gtk.hBoxNew False 0
  modeLabel <- Gtk.labelNew $ Just "Mode: Replace"
  currentLabel <- Gtk.labelNew $ Just "(0,0)"
  Gtk.boxPackEnd hboxInfoLine currentLabel Gtk.PackNatural 3
  Gtk.boxPackStart hboxInfoLine modeLabel Gtk.PackNatural 3
  return hboxInfoLine
