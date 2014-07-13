{- |
Module      :  MenuBar.hs
Description :  .
Maintainer  :  Chritoph Graebnitz (c)
License     :  MIT

Stability   :  stable

The MenuBar-module depicts the menu bar at he top of the main window.
-}
module MenuBar (
  create
               )
  where
    
    -- imports --

    -- functions --

import TextArea
import TextAreaContent
import qualified Highlighter as HIGH
import qualified Execute as EXE
import Graphics.UI.Gtk as Gtk
import qualified Control.Exception as Exc
import System.Exit
import Data.Maybe
import Control.Monad.IO.Class
import Data.List
import Data.IORef

{-
Handels the button press and open or saves a file
-}
fileChooserEventHandler :: Window 
  -> TextArea
  -> FileChooserDialog 
  -> ResponseId
  -> String
  -> Gtk.TextBuffer
  -> Gtk.TextBuffer
  -> IO()
fileChooserEventHandler window area fileChooser response mode inputB outputB
  |response == ResponseOk = do
    dir <- fileChooserGetFilename fileChooser
    let path = fromJust dir
    set window[windowTitle := path] 
    let contRef = textAreaContent area
    areaContent <- readIORef contRef
    case mode of
      "OpenFile" -> do
        content <- readFile path
        newAreaContent <- deserialize content inputB outputB
        setTextAreaContent area newAreaContent
        widgetDestroy fileChooser
        return()
      "SaveFile" -> do
        code <- (serialize areaContent)
        writeFile path code
        widgetDestroy fileChooser
        return()
  |response == ResponseCancel = do
    widgetDestroy fileChooser
    return ()
  |otherwise = return ()
  
--checking for a legal path in window title to save whitout dialog
saveFile :: Window -> TextArea -> Gtk.TextBuffer -> Gtk.TextBuffer -> IO Bool
saveFile window area inputB outputB = do
  let contRef = textAreaContent area
  areaContent <- readIORef contRef
  code <- serialize areaContent
  dir <- get window windowTitle
  if "/" `isInfixOf` dir && not("/" `isSuffixOf` dir)
  then do
    writeFile dir code
    return True
  else fileDialog window area "SaveFile" inputB outputB >> return True

{-
TODO Refactor text to an 'link' to the entry text
for the ability to save files
Passes the enventhandler for fileDialog and starts it
-}
runFileChooser :: Window
  -> TextArea
  -> FileChooserDialog
  -> String
  -> Gtk.TextBuffer
  -> Gtk.TextBuffer
  -> IO()
runFileChooser window area fileChooser mode inputB outputB= do
  on fileChooser response hand
  dialogRun fileChooser
  return()
  where 
    hand resp = fileChooserEventHandler window area fileChooser resp mode inputB outputB

{-
Setup a file chooser with modes OpenFile and SaveFile
TODO Refactor text to an 'link' to the entry text
for the ability to save files
-}
fileDialog :: Window
  -> TextArea
  -> String
  -> Gtk.TextBuffer
  -> Gtk.TextBuffer
  -> IO()
fileDialog window area mode inputB outputB= do
  case mode of
    "OpenFile" -> do
      fileChooser <- fileChooserDialogNew 
        (Just mode)
        (Just window)
        FileChooserActionOpen
        [("open",ResponseOk),("cancel",ResponseCancel)]
      runFileChooser window area fileChooser mode inputB outputB
    "SaveFile" -> do
      fileChooser <- fileChooserDialogNew
        (Just mode)
        (Just window)
        FileChooserActionSave
        [("save",ResponseOk),("cancel",ResponseCancel)]
      fileChooserSetDoOverwriteConfirmation fileChooser True
      runFileChooser window area fileChooser mode inputB outputB
  return ()
  

{-
TODO Refactor text to an 'link' to the entry text
for the ability to save files
Setups the menu
-}
create :: Window
  -> TextArea
  -> TextBuffer
  -> TextBuffer
  -> IO MenuBar
create window area output input= do
  menuBar <- menuBarNew-- container for menus

  menuFile <- menuNew
  menuHelp <- menuNew

  menuFileItem <- menuItemNewWithLabel "File"
  menuOpenItem <- menuItemNewWithLabel "open crtl+o"
  menuSaveItem <- menuItemNewWithLabel "save ctrl+s"
  menuCloseItem <- menuItemNewWithLabel "quit ctrl+s"
  menuCompileItem <- menuItemNewWithLabel "compile ctrl+shift+F5"
  menuCompileAndRunItem <- menuItemNewWithLabel "compile and run ctrl+F5"
  menuHelpItem <- menuItemNewWithLabel "Help"
  menuAboutItem <- menuItemNewWithLabel "About"
  --Bind the subemenu to menu
  menuItemSetSubmenu menuFileItem menuFile
  menuItemSetSubmenu menuHelpItem menuHelp
  --File and Help menu
  menuShellAppend menuBar menuFileItem
  menuShellAppend menuBar menuHelpItem
  --Insert items in menus
  menuShellAppend menuFile menuOpenItem
  menuShellAppend menuFile menuSaveItem
  menuShellAppend menuFile menuCloseItem
  menuShellAppend menuFile menuCompileItem
  menuShellAppend menuFile menuCompileAndRunItem
  menuShellAppend menuHelp menuAboutItem
  --setting actions for the menu
  on menuOpenItem menuItemActivate (fileDialog 
    window 
    area
    "OpenFile"
    input
    output
    )
  on menuSaveItem menuItemActivate (saveFile
    window
    area
    input
    output >> return())
  on menuCloseItem menuItemActivate mainQuit
  on menuCompileItem menuItemActivate 
    (compileOpenFile window output input >> return ())
  on menuCompileAndRunItem menuItemActivate $ compileAndRun window output input
    
  --setting shortcuts in relation to menuBar
  on window keyPressEvent $ do
    modi <- eventModifier
    key <- eventKeyName
    liftIO $ case modi of
      [Control] -> case key of
        "q" -> mainQuit >> return True
        "s" -> saveFile window area input output >> return True
        "o" -> fileDialog
          window
          area
          "OpenFile"
          input
          output >> return True
        "F5" -> compileAndRun window output input >> return True
        _ -> return False
      [Shift,Control] -> case key of
        "F5" -> compileOpenFile window output input >> return True
        _ -> return False
      _ -> return False
  return menuBar

-- | It compiles and interprets the rail source.
-- Using inputbuffer for input and outputbuffer for programm output
compileAndRun :: Window
  -> TextBuffer
  -> TextBuffer 
  -> IO ()
compileAndRun window output input = do
  (exeName,msg) <- compileOpenFile window output input
  inP <- get input textBufferText
  (exitCode,out,err) <- EXE.executeRail exeName inP
  if exitCode == (ExitSuccess)
  then textBufferSetText output out
  else textBufferSetText output (msg++"\n"++out++"\n"++err)

-- | This Function invokes the compilation and linking of the open rail source.
-- It also puts stdout and sterr from linking and compiling to bufferOut.
compileOpenFile ::Window
  -> TextBuffer
  -> TextBuffer 
  -> IO (String,String) --name of linked file and msg from compiler and llvm-link
compileOpenFile window output input = do
  path <- get window windowTitle
  let compiledPath = ((((takeWhile(/='.')).reverse.(takeWhile(/='/')).reverse)path)++".ll")
  textBufferSetText output "Compiling Execute"
  (exitCode,out,err) <- EXE.compile path compiledPath
  if exitCode == (ExitSuccess)
  then do
    let exeName = takeWhile (/='.') compiledPath
    textBufferSetText output "Compiling succsessful"
    (exitCode,out,err) <- EXE.linkLlvm compiledPath exeName
    if exitCode == (ExitSuccess)
    then do
      let msg = "Compiling succsessful\n"++out++"\n"++err
      textBufferSetText output (msg)
      return (exeName,msg)
    else do
      let msg = ("llvm-link failed:\n"++out++"\n"++err)
      textBufferSetText output msg
      return (exeName,msg)
  else do 
    let msg = "Compiling failed: "++['\n']++out++err
    textBufferSetText output msg
    return ("",msg)


