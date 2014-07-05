{- |
Module      :  MenuBar.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

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
import Execute
import Graphics.UI.Gtk
import qualified Control.Exception as Exc
import System.Exit
import Data.Maybe
import Control.Monad.IO.Class
import Data.List
import Data.IORef

{-TODO Refactor text to an 'link' to the entry text
  for the ability to save files
Handels the button press and open or saves a file
-}
fileChooserEventHandler :: Window 
  -> TextArea
  -> FileChooserDialog 
  -> ResponseId
  -> String
  -> IO()
fileChooserEventHandler window area fileChooser response mode
  |response == ResponseOk = do
    dir <- fileChooserGetFilename fileChooser
    let path = fromJust dir
    set window[windowTitle := path] 
    let contRef = textAreaContent area
    areaContent <- readIORef contRef
    case mode of
      "OpenFile" -> do
        content <- readFile path
        newAreaContent <- deserialize content
        setTextAreaContent area newAreaContent
--        syntaxHighlighting areaContent
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
saveFile :: Window -> TextArea -> IO Bool
saveFile window area = do
  let contRef = textAreaContent area
  areaContent <- readIORef contRef
  code <- serialize areaContent
  dir <- get window windowTitle
  if "/" `isInfixOf` dir && not("/" `isSuffixOf` dir)
  then do
    writeFile dir code
    return True
  else fileDialog window area "SaveFile" >> return True

{-
TODO Refactor text to an 'link' to the entry text
for the ability to save files
Passes the enventhandler for fileDialog and starts it
-}
runFileChooser :: Window
  -> TextArea
  -> FileChooserDialog
  -> String
  -> IO()
runFileChooser window area fileChooser mode = do
  on fileChooser response hand
  dialogRun fileChooser
  return()
  where 
    hand resp = fileChooserEventHandler window area fileChooser resp mode

{-
Setup a file chooser with modes OpenFile and SaveFile
TODO Refactor text to an 'link' to the entry text
for the ability to save files
-}
fileDialog :: Window
  -> TextArea
  -> String
  -> IO()
fileDialog window area mode = do
  case mode of
    "OpenFile" -> do
      fileChooser <- fileChooserDialogNew 
        (Just mode)
        (Just window)
        FileChooserActionOpen
        [("open",ResponseOk),("cancel",ResponseCancel)]
      runFileChooser window area fileChooser mode
    "SaveFile" -> do
      fileChooser <- fileChooserDialogNew
        (Just mode)
        (Just window)
        FileChooserActionSave
        [("save",ResponseOk),("cancel",ResponseCancel)]
      fileChooserSetDoOverwriteConfirmation fileChooser True
      runFileChooser window area fileChooser mode
  return ()
  

{-
TODO Refactor text to an 'link' to the entry text
for the ability to save files
Setups the menu
-}
create :: Window
  -> TextArea
  -> TextBuffer
  -> IO MenuBar
create window area output= do
  menuBar <- menuBarNew-- container for menus

  menuFile <- menuNew
  menuHelp <- menuNew

  menuFileItem <- menuItemNewWithLabel "File"
  menuOpenItem <- menuItemNewWithLabel "open crtl+o"
  menuSaveItem <- menuItemNewWithLabel "save ctrl+s"
  menuCloseItem <- menuItemNewWithLabel "quit ctrl+s"
  menuCompileItem <- menuItemNewWithLabel "compile ctrl+F5"
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
  menuShellAppend menuHelp menuAboutItem
  --setting actions for the menu
  on menuOpenItem menuItemActivate (fileDialog 
    window 
    area
    "OpenFile")
  on menuSaveItem menuItemActivate (saveFile
    window
    area >> return())
  on menuCloseItem menuItemActivate mainQuit
  on menuCompileItem menuItemActivate 
    (compileOpenFile window area output >> return ())
  --setting shortcuts in relation to menuBar
  on window keyPressEvent $ do
    modi <- eventModifier
    key <- eventKeyName
    liftIO $ case modi of
      [Control] -> case key of
        "q" -> mainQuit >> return True
        "s" -> saveFile window area  >> return True
        "o" -> fileDialog
          window
          area
          "OpenFile" >> return True
        "F5" -> compileOpenFile window area output
        _ -> return False
      _ -> return False
  return menuBar

compileOpenFile ::Window
  -> TextArea
  -> TextBuffer 
  -> IO Bool
compileOpenFile window area output = do
  textBufferSetText output "Compiling Execute"
  (exitCode,out,err) <-compile window
  if exitCode == (ExitSuccess)
  then textBufferSetText output "Compiling succsessful"
  else textBufferSetText output ("Compiling failed: "++['\n']++err)
  return True


