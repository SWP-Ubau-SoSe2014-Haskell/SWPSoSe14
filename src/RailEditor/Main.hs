module Main where

import Graphics.UI.Gtk
import Data.Map as Map
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.Maybe
import Menu
import TextArea
import Control.Concurrent

main :: IO()
main = do
    initGUI
    splashScreen <- getSplashScreen
    window <- windowNew
    label <- labelNewWithMnemonic "Hi" 

    bufferIn <- textBufferNew Nothing
    bufferOut <- textBufferNew Nothing
    bufferStackFunc <- textBufferNew Nothing
    bufferStackVar <- textBufferNew Nothing

    labelIn <- labelNewWithMnemonic "Input:"
    viewIn <- textViewNewWithBuffer bufferIn
    labelOut <- labelNewWithMnemonic "Output:"
    viewOut <- textViewNewWithBuffer bufferOut
    labelStackFunc <- labelNewWithMnemonic "Functionstack"
    viewStackFunc <- textViewNewWithBuffer bufferStackFunc
    labelStackVar <- labelNewWithMnemonic "Variablestack"
    viewStackVar <- textViewNewWithBuffer bufferStackVar

    buttonPopUpIn <- buttonNewWithLabel ""
    setButtonProps buttonPopUpIn
    buttonPopUpOut <- buttonNewWithLabel ""
    setButtonProps buttonPopUpOut
    buttonPopUpStackF <- buttonNewWithLabel ""
    setButtonProps buttonPopUpStackF
    buttonPopUpStackV <- buttonNewWithLabel ""
    setButtonProps buttonPopUpStackV

    layout <- layoutNew Nothing Nothing
    lwin <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy lwin PolicyAutomatic PolicyAutomatic
    containerAdd lwin layout
    textArea <- textAreaNew layout 10 10

    onClicked buttonPopUpIn $ postGUIAsync $ textViewWindowShow bufferIn "Input"
    onClicked buttonPopUpOut $ postGUIAsync $ textViewWindowShow bufferOut "Output"
    onClicked buttonPopUpStackF $ postGUIAsync $ textViewWindowShow bufferStackFunc "Function-Stack"
    onClicked buttonPopUpStackV $ postGUIAsync $ textViewWindowShow bufferStackVar "Variable-Stack"

    hboxLabelButtonIn <- hBoxNew False 0
    boxPackStart hboxLabelButtonIn labelIn PackNatural 1
    boxPackEnd hboxLabelButtonIn buttonPopUpIn PackNatural 0

    hboxLabelButtonOut <- hBoxNew False 0
    boxPackStart hboxLabelButtonOut labelOut PackNatural 1
    boxPackEnd hboxLabelButtonOut buttonPopUpOut PackNatural 0

    hboxInfoLine <- hBoxNew False 0
    modeLabel <- labelNew $ Just "Mode: Replace"
    currentLabel <- labelNew $ Just "(0,0)"
    boxPackEnd hboxInfoLine currentLabel PackNatural 3
    boxPackStart hboxInfoLine modeLabel PackNatural 3

    boxStackFunc <- vBoxNew False 0
    hboxLabelButtonFunc <- hBoxNew False 0
    boxPackStart hboxLabelButtonFunc labelStackFunc PackNatural 0
    boxPackEnd hboxLabelButtonFunc buttonPopUpStackF PackNatural 10
    boxPackStart boxStackFunc hboxLabelButtonFunc PackNatural 0
    swinStackF  <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy swinStackF PolicyAutomatic PolicyAutomatic
    containerAdd swinStackF viewStackFunc
    boxPackStart boxStackFunc swinStackF PackGrow 0

    boxStackVar <- vBoxNew False 0
    hboxLabelButtonVar <- hBoxNew False 0
    boxPackStart hboxLabelButtonVar labelStackVar PackNatural 0
    boxPackEnd hboxLabelButtonVar buttonPopUpStackV PackNatural 10
    boxPackStart boxStackVar hboxLabelButtonVar PackNatural 0
    swinStackV <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy swinStackV PolicyAutomatic PolicyAutomatic
    containerAdd swinStackV viewStackVar
    boxPackStart boxStackVar swinStackV PackGrow 0

    boxStack <- hBoxNew True 0
    boxPackStart boxStack boxStackFunc PackGrow 2
    boxPackStart boxStack boxStackVar PackGrow 2

    boxView <- vBoxNew False 0
    boxLay <- hBoxNew False 0
    boxPackStart boxView hboxLabelButtonIn PackNatural 2
    swinIn <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy swinIn PolicyAutomatic PolicyAutomatic
    containerAdd swinIn viewIn
    boxPackStart boxView swinIn PackGrow 0
    inSap <- hSeparatorNew
    boxPackStart boxView inSap PackNatural 2
    boxPackStart boxView hboxLabelButtonOut PackNatural 2
    swinOut <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy swinOut PolicyAutomatic PolicyAutomatic
    containerAdd swinOut viewOut
    boxPackStart boxView swinOut PackGrow 1
    outSap <- hSeparatorNew
    boxPackStart boxView outSap PackNatural 2
    boxPackStart boxView boxStack PackGrow 1
    boxPackStart boxLay lwin PackGrow 1
    vSep <- vSeparatorNew
    boxPackStart boxLay vSep PackNatural 2
    boxPackEnd boxLay boxView PackNatural 1

    table <- tableNew 5 1 False

    menuBar <- createMenu window textArea bufferOut
    extraBar <- createExtraBar

    vSepa <- hSeparatorNew

    tableAttach table menuBar 0 1 0 1 [Fill] [Fill] 0 0
    tableAttach table extraBar 0 1 1 2 [Fill] [Fill] 0 0
    tableAttach table boxLay 0 1 2 3 [Expand,Fill] [Expand,Fill] 0 0
    tableAttach table vSepa 0 1 3 4 [Fill] [Fill] 0 0
    tableAttach table hboxInfoLine 0 1 4 5 [Fill] [Fill] 2 2

    set window [containerChild := table, windowDefaultHeight := 550, windowDefaultWidth := 850, windowWindowPosition := WinPosCenter]
    onDestroy window mainQuit
    widgetShowAll window
    widgetDestroy splashScreen
    mainGUI
    return ()

createExtraBar = do
    extraBar <- menuBarNew

    image <- imageNewFromStock stockExecute IconSizeMenu
    run <- imageMenuItemNewWithLabel ""
    imageMenuItemSetImage run image
    menuShellAppend extraBar run
    imageD <- imageNewFromStock stockGoForward IconSizeMenu

    debugg <- imageMenuItemNewWithLabel ""
    imageMenuItemSetImage debugg imageD
    menuShellAppend extraBar debugg

    mode <- menuNew
    replaceMode <- radioMenuItemNewWithLabel "replace"
    insertMode <- radioMenuItemNewWithLabelFromWidget replaceMode "insert"
    smartMode <- radioMenuItemNewWithLabelFromWidget replaceMode "smart"

    modeItem <- menuItemNewWithLabel "mode"
    menuItemSetSubmenu modeItem mode

    menuShellAppend extraBar modeItem

    menuShellAppend mode replaceMode
    menuShellAppend mode insertMode
    menuShellAppend mode smartMode


    return extraBar

setButtonProps button = do
    image <- imageNewFromFile "full.png"
    buttonSetImage button image
    buttonSetImagePosition button PosRight

textViewWindowShow textBuffer title = do
    window <- windowNew
    windowSetDefaultSize window 400 300
    windowSetPosition window WinPosCenter
    swin <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy swin PolicyAutomatic PolicyAutomatic
    textView <- textViewNewWithBuffer textBuffer
    containerAdd swin textView
    set window [containerChild := swin, windowTitle := title]
    widgetShowAll window
    return ()

getSplashScreen :: IO Window
getSplashScreen = do
    splashScreen <- windowNew
    set splashScreen [windowDefaultHeight := 200, windowDefaultWidth := 400, windowWindowPosition := WinPosCenter, windowTitle := "Starting Editor"]
    windowSetDefaultSize splashScreen 400 200 
    windowSetPosition splashScreen WinPosCenter
    layout <- layoutNew Nothing Nothing
    label <- labelNewWithMnemonic "Rail Editor starting ..."
    layoutPut layout label 30 30
    set splashScreen [containerChild := layout]
    widgetShowAll splashScreen
    return splashScreen

