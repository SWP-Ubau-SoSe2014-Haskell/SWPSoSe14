module Execute where

import Graphics.UI.Gtk
import System.Process

compile :: Window
  -> String
  -> IO ProcessHandle
compile window outPutName = do
  path <- get window windowTitle
  runProcess "dist/build/SWPSoSe14/SWPSoSe14" 
    ["-c","-i",path,"-o",outPutName]
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
