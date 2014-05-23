import Graphics.UI.Gtk
import Data.Maybe
--Menu Setup

openFile :: Window -> IO()
openFile window = do
		fileChooser <- fileChooserDialogNew (Just "OpenFile")(Just window) FileChooserActionOpen [("open",ResponseOk),("cancel",ResponseCancel)]
		--TODO new response Function with pattenmatching on signals
		on fileChooser response (\ResponseOk -> do
										dir <- fileChooserGetFilename fileChooser
										--putStrLn(fromJust dir)
										content <- readFile (fromJust dir)
										putStrLn(content)
										return()
								\ResponseCancel -> do
										return())
		dialogRun fileChooser
		return ()
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
		on menuOpenItem menuItemActivate (openFile window)
		return menuBar

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