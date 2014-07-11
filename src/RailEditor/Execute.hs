module Execute where

import Graphics.UI.Gtk
import System.Process
import System.Exit

-- Compiles the open file
compile :: String -- Input filepath
  -> String -- output filepath
  -> IO (ExitCode,String,String)
compile input output = do
  readProcessWithExitCode "dist/build/RailCompiler/RailCompiler"
    ["-c","-i",input,"-o",output] ""

--linkes copmiled railcode with llvm
linkLlvm :: String --Compiled code
  -> String -- executable path
  -> IO (ExitCode,String,String)
linkLlvm compiledCode exe = do
--src/RailCompiler/*.ll dose not work.
  readProcessWithExitCode "llvm-link" [compiledCode,"src/RailCompiler/cmp.ll",
    "src/RailCompiler/linked_stack.ll","src/RailCompiler/math.ll",
    "src/RailCompiler/stack.ll","src/RailCompiler/string.ll","-o",exe] ""

--executes the executable
executeRail :: String
  -> String
  -> IO (ExitCode,String,String)
executeRail exeName input = do
  readProcessWithExitCode "lli" [exeName] input
  

