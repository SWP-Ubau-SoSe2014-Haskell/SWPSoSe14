import Graphics.UI.Gtk
import Data.Maybe
import Control.Monad.IO.Class
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
				putStrLn(content)
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
	on fileChooser response (\resp -> (fileChooserEventHandler  fileChooser text resp mode))
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
				"q" -> mainQuit >> return(True);
				"s" -> fileDialog window "ENTRY-CONTENT-STUB" "SaveFile" >> return(True);
				"o" -> fileDialog window "ENTRY-CONTENT-STUB" "OpenFile" >> return(True);
				_ -> return(False);
			};
			_ -> return(False);
		}
	return (menuBar)

main :: IO ()
main = do
	initGUI
	
	window <- windowNew
	windowSetDefaultSize window 600 480
	
	menuBar <- createMenu window
	table <- tableNew 4 4 True
	tableAttach table menuBar 0 1 0 1 [] [] 0 0

	set window [ windowTitle := "Raileditor"
               , containerChild := table]
	on window objectDestroy mainQuit
	widgetShowAll window
	mainGUI