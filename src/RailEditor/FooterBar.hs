{- |
Module      :  FooterBar.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

The FooterBar-module depicts the footer bar at the bottom of the main-window.
-}
module FooterBar (
  create,
  Footer,
  getContainer,
  setPosition,
  setMode
                 )
  where

    -- imports --
import qualified KeyHandler as KH
import qualified Graphics.UI.Gtk as Gtk
    -- functions --

-- | encapsulates information about the footer
data Footer = Foot {getContainer :: Gtk.HBox,
                    getCurrentPositionLabel :: Gtk.Label,
                    getModeLabel :: Gtk.Label}

-- | sets the label displaying the current position
setPosition :: Footer -> (Int,Int) -> IO ()
setPosition footer (x,y) =
  Gtk.labelSetText (getCurrentPositionLabel footer) ("(" ++ show x ++ "," ++ show y ++ ")")

-- | set the label displaying the current mode
setMode :: Footer -> KH.InputMode -> IO ()
setMode footer KH.Replace = Gtk.labelSetText (getModeLabel footer) "Mode: Replace"
setMode footer KH.Insert = Gtk.labelSetText (getModeLabel footer) "Mode: Insert"
setMode footer KH.Smart = Gtk.labelSetText (getModeLabel footer) "Mode: Smart"

-- | creates a footer
create = do
  hboxInfoLine <- Gtk.hBoxNew False 0

  modeLabel <- Gtk.labelNew $ Just "Mode: Insert"
  currentLabel <- Gtk.labelNew $ Just "(0,0)"

  Gtk.boxPackEnd hboxInfoLine currentLabel Gtk.PackNatural 3
  Gtk.boxPackStart hboxInfoLine modeLabel Gtk.PackNatural 3
  return $ Foot hboxInfoLine currentLabel modeLabel
