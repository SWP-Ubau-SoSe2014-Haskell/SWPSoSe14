module Execute where

import Graphics.UI.Gtk
import System.Process
import System.Exit

compile :: Window
  -> IO (ExitCode,String,String)
compile window = do
  path <- get window windowTitle
  readProcessWithExitCode "dist/build/SWPSoSe14/SWPSoSe14" 
    ["-c","-i",path,"-o",((reverse.(takeWhile(/='/')).reverse)path)] ""
