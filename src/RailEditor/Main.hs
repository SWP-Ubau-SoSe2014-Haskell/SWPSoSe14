module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Maybe
import Data.Bits

main::IO()
main = start gui


gui::IO()
gui = do
        mainFrame <- frame [text := "RailEditor"]
        set mainFrame []
        documentView <- textCtrlRich mainFrame []
        set mainFrame [layout := fill (widget  documentView)]
        set mainFrame [outerSize := (sz 640 480)]
        set documentView [wrap := WrapNone]
								
        --Setup the menu 
        mnu <- menuPane [text := "file"]
        filePath <- varCreate Nothing
        menuItem mnu [text := "&Open\tCtrl+O",
                      help := "Opens an existing document",
                      on command := openFile mainFrame  documentView
                      ]
        menuItem mnu [text := "&Save\tCtrl+X",
                      help := "Saves a file in the choosen directory",
                      on command := saveFile mainFrame  documentView
                      ]
        menuItem mnu [text := "&Quit\tCtrl+Q",
                      help := "Closes the application",
                      on command := wxcAppExit]
        mnuHelp <- menuHelp []      
        set mainFrame[menuBar := [mnu,mnuHelp],visible := True]
        
        return ()
 
--Saves the content of an TextCtrl to a file     
saveFile:: Frame() -> TextCtrl() -> IO()     
saveFile mainFrame textField = do
                                path <- fileSaveDialog mainFrame False True "save File" [("Any file",["*.*"])] "" ""
                                textCtrlSaveFile textField (fromJust path)
                                return()

--Opens a file and displays the content in textCtrl
openFile :: Frame() -> TextCtrl() ->IO()
openFile mainFrame textField = do
                                 path <- fileOpenDialog mainFrame False False "openFile" [("Any file",["*.*"])] "" ""
                                 textCtrlLoadFile textField (fromJust path)
                                 return()
                  