module Main where

import Graphics.UI.Gtk
import Data.Map as Map
import Control.Monad.Trans (liftIO)
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe

data TextArea = TextArea Layout (IORef (Int,Int)) (IORef (Map.Map (Int,Int) Entry)) (IORef (Int,Int))

getLayout (TextArea layout _ _ _) = layout

getPointerToCurrentInFocus (TextArea _ current _ _) = current

getPointerToEntryMap (TextArea _ _ map _) = map

getPointerToSize (TextArea _ _ _ size) = size

main :: IO()
main = do
    initGUI
    window <- windowNew
    layout <- layoutNew Nothing Nothing
    menuBar <- createMenu window
    layoutPut layout menuBar 0 0
    textArea <- textAreaNew layout 10 10
    set window [ windowDefaultWidth  := 500,
                 windowDefaultHeight := 200,
                 containerChild      := layout ]
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
    return ()

textAreaNew :: Layout -> Int -> Int -> IO(TextArea)
textAreaNew layout x y = do
    currentInFocus <- newIORef (0,0)
    hashMap <- newIORef Map.empty
    size <- newIORef (0,0)
    let area =  TextArea layout currentInFocus hashMap size
    createTextArea area x y
    return area

createTextArea :: TextArea -> Int -> Int -> IO()
createTextArea area@(TextArea layout current hmap size) x y = do
    createTextAreaH area 0 (pred x) 0 (pred y)
    writeIORef size (x,y)
    return ()

createTextAreaH :: TextArea -> Int -> Int -> Int -> Int -> IO()
createTextAreaH area@(TextArea layout current hamp size) xnr xnrS ynr ynrS = do
    (maxX,maxY) <- readIORef size
    if xnr == xnrS && ynr == ynrS
    then do 
        entryInsert area xnrS xnrS
    else if xnr == xnrS && ynr < ynrS
        then do
            entryInsert area  xnr ynr
            createTextAreaH area 0 xnrS (succ ynr) ynrS
        else do
            entryInsert area xnr ynr
            createTextAreaH area (succ xnr) xnrS ynr ynrS

entryInsert :: TextArea -> Int -> Int -> IO()
entryInsert area@(TextArea layout current hMap size) x y = do
    entry <- entryNew
    set entry [entryWidthChars := 1]
    entrySetMaxLength entry 1
    entrySetHasFrame entry False
    entry `on` focusInEvent $ tryEvent $ liftIO $ writeIORef current (x,y)
    layoutPut layout entry (x*10) ((18*y)+20)
    hamp <- readIORef hMap
    let hMapN = Map.insert (x,y) entry hamp
    writeIORef hMap hMapN
    on entry keyPressEvent $ do -- TODO: handle Shift_L/Shift_R + dollar,ISO_Level3_Shift + backslash .....
        key <- eventKeyName
        liftIO $ if isSimpleChar key
            then do
                set entry [entryText := key]
                hmap <- readIORef hMap
                let nextEntry = Map.lookup (x+1,y) hmap
                if not $ isJust nextEntry
                then do
                    let nextEntry = Map.lookup (0,y+1) hmap
                    if not $ isJust nextEntry
                    then return False
                    else do
                        let nEntry = fromJust nextEntry
                        widgetGrabFocus nEntry
                        return True
                else do
                    let nEntry = fromJust nextEntry
                    widgetGrabFocus nEntry
                    return True
            else do
                return False
    return ()
            where isSimpleChar x = elem x $ Prelude.map (\x -> [x])  (['a'..'z']++['A'..'Z']++" $\\/|-+x*")

--This is the main form for the raileditor insert all functionality here.

--TODO Refactor text to an 'link' to the entry text for the ability to save files
--Handels the button press and open or saves a file
fileChooserEventHandler :: FileChooserDialog -> String -> ResponseId -> String -> IO()
fileChooserEventHandler fileChooser text response mode 
	| response == ResponseOk = do
		case mode of
			"OpenFile" -> do
				dir <- fileChooserGetFilename fileChooser
				content <- readFile (fromJust dir)
				putStrLn content
				return()
			"SaveFile" -> do
				dir <- fileChooserGetFilename fileChooser
				writeFile (fromJust dir) text
		widgetDestroy fileChooser
		return()
	| response == ResponseCancel = do
			widgetDestroy fileChooser
			return ()

--TODO Refactor text to an 'link' to the entry text for the ability to save files
--Passes the enventhandler for fileDialog and starts it
runFileChooser :: String -> FileChooserDialog -> String -> IO()
runFileChooser text fileChooser mode = do
	on fileChooser response (\resp -> fileChooserEventHandler  fileChooser text resp mode)
	dialogRun fileChooser
	return()


--Setup a file chooser with modes OpenFile and SaveFile 
--TODO Refactor text to an 'link' to the entry text for the ability to save files
fileDialog :: Window -> String -> String -> IO()
fileDialog window text mode = do
	case mode of 
		"OpenFile" -> do 
				fileChooser <- fileChooserDialogNew (Just mode)(Just window)FileChooserActionOpen[("open",ResponseOk),("cancel",ResponseCancel)]
				runFileChooser text fileChooser mode
		"SaveFile" -> do
			fileChooser <- fileChooserDialogNew (Just mode)(Just window) FileChooserActionSave [("save",ResponseOk),("cancel",ResponseCancel)]
			fileChooserSetDoOverwriteConfirmation fileChooser True
			runFileChooser text fileChooser mode
	return ()

--TODO Refactor text to an 'link' to the entry text for the ability to save files
--Setups the menu
createMenu :: Window -> IO MenuBar
createMenu window = do
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
	on menuOpenItem menuItemActivate (fileDialog window "ENTRY-CONTENT-STUB" "OpenFile")
	on menuSaveItem menuItemActivate (fileDialog window "ENTRY-CONTENT-STUB" "SaveFile")
	on menuCloseItem menuItemActivate mainQuit
	--setting shortcuts in relation to menuBar
	on window keyPressEvent $ 
		do 
		modi <- eventModifier
		key <- eventKeyName
		liftIO $ case modi of{
			[Control] -> case key of{
				"q" -> mainQuit >> return True;
				"s" -> fileDialog window "ENTRY-CONTENT-STUB" "SaveFile" >> return True;
				"o" -> fileDialog window "ENTRY-CONTENT-STUB" "OpenFile" >> return True;
				_ -> return False;
			};
			_ -> return False;
		}
	return menuBar
