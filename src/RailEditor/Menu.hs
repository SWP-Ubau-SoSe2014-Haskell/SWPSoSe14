module Menu where

import TextArea
import Execute
import Graphics.UI.Gtk
import Data.Maybe
import Control.Monad.IO.Class
import Data.List

{-TODO Refactor text to an 'link' to the entry text
  for the ability to save files
Handels the button press and open or saves a file
-}
fileChooserEventHandler :: Window 
  -> FileChooserDialog 
  -> String
  -> ResponseId
  -> String
  -> IO()
fileChooserEventHandler window fileChooser text response mode
  |response == ResponseOk = do
    case mode of
      "OpenFile" -> do
        dir <- fileChooserGetFilename fileChooser
        path <- return $ (fromJust dir)
        set window[windowTitle := path] 
        content <- readFile path
        putStrLn content
        widgetDestroy fileChooser
        return()
      "SaveFile" -> do
        dir <- fileChooserGetFilename fileChooser
        path <- return $ (fromJust dir)
        set window[windowTitle := path]
        writeFile path text
        widgetDestroy fileChooser
        return()
  |response == ResponseCancel = do
    widgetDestroy fileChooser
    return ()
  |otherwise = return ()
--checking for a legal path in window title to save whitout dialog
saveFile :: Window -> IO Bool
saveFile window = do 
  dir <- get window windowTitle
  if  (isInfixOf "/" dir) && not(isSuffixOf "/" dir)
  then do
    writeFile dir "ENTRY-CONTENT-STUB"
    return True
  else fileDialog window "ENTRY-CONTENT-STUB" "SaveFile" >> return True

{-
TODO Refactor text to an 'link' to the entry text
for the ability to save files
Passes the enventhandler for fileDialog and starts it
-}
runFileChooser :: Window
  -> String
  -> FileChooserDialog
  -> String
  -> IO()
runFileChooser window text fileChooser mode = do
  on fileChooser response hand
  dialogRun fileChooser
  return()
  where 
    hand = (\resp -> fileChooserEventHandler 
      window 
      fileChooser
      text
      resp
      mode)

{-
Setup a file chooser with modes OpenFile and SaveFile
TODO Refactor text to an 'link' to the entry text
for the ability to save files
-}
fileDialog :: Window
  -> String
  -> String
  -> IO()
fileDialog window text mode = do
  case mode of
    "OpenFile" -> do
      fileChooser <- fileChooserDialogNew 
        (Just mode)
        (Just window)
        FileChooserActionOpen
        [("open",ResponseOk),("cancel",ResponseCancel)]
      runFileChooser window text fileChooser mode
    "SaveFile" -> do
      fileChooser <- fileChooserDialogNew
        (Just mode)
        (Just window)
        FileChooserActionSave
        [("save",ResponseOk),("cancel",ResponseCancel)]
      fileChooserSetDoOverwriteConfirmation fileChooser True
      runFileChooser window text fileChooser mode
  return ()
  

{-
TODO Refactor text to an 'link' to the entry text
for the ability to save files
Setups the menu
-}
createMenu :: Window
  -> TextArea
  -> IO MenuBar
createMenu window area= do
  menuBar <- menuBarNew-- container for menus

  menuFile <- menuNew
  menuHelp <- menuNew

  menuFileItem <- menuItemNewWithLabel "File"
  menuOpenItem <- menuItemNewWithLabel "open crtl+o"
  menuSaveItem <- menuItemNewWithLabel "save ctrl+s"
  menuCloseItem <- menuItemNewWithLabel "quit ctrl+s"
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
  menuShellAppend menuHelp menuAboutItem
  --setting actions for the menu
  on menuOpenItem menuItemActivate (fileDialog 
    window 
    "ENTRY-CONTENT-STUB"
    "OpenFile")
  on menuSaveItem menuItemActivate (saveTextAreaToFile
    window
    area)
  on menuCloseItem menuItemActivate mainQuit
  --setting shortcuts in relation to menuBar
  on window keyPressEvent $ do
    modi <- eventModifier
    key <- eventKeyName
    liftIO $ case modi of
      [Control] -> case key of
        "q" -> mainQuit >> return True
        "s" -> saveTextAreaToFile window area  >> return True
        "o" -> fileDialog
          window
          "ENTRY-CONTENT-STUB"
          "OpenFile" >> return True
        _ -> return False
      _ -> return False
  return menuBar

saveTextAreaToFile window area = do
    areaText <- serializeTextAreaContent area
    fileDialog window areaText "SaveFile"
    return ()

