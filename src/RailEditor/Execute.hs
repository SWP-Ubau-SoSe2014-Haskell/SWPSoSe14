module Execute where

import Graphics.UI.Gtk
import System.Process
import System.Exit

-- Compiles the open file
compile :: Window --Main Window which contain the path to the open File
  -> IO (ExitCode,String,String)
compile window = do
  path <- get window windowTitle
  readProcessWithExitCode "dist/build/RailCompiler/RailCompiler" 
    ["-c","-i",path,"-o",((((takeWhile(/='.')).reverse.(takeWhile(/='/')).reverse)path)++".ll")] ""
