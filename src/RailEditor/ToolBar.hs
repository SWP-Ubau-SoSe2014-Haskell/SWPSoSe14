{- |
Module      :  ToolBar.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

The ToolBar-module depicts the tool bar at the top of the main window below the menu bar.
-}
module ToolBar (
  create
               )
  where
    
    -- imports --

import Data.IORef
import qualified KeyHandler as KH
import qualified FooterBar as FB
import qualified InteractionField as IDF
import qualified TextArea as TA
import qualified Graphics.UI.Gtk as Gtk
import qualified Interpreter as IN
import qualified Lexer
import qualified TextAreaContent as TAC
import qualified Data.Map as Map
import Control.Monad
    -- functions --

-- | creates a toolbar
create area footer interDT= do

    toolBar <- Gtk.menuBarNew

    -- create step button
    image <- Gtk.imageNewFromStock Gtk.stockMediaPlay Gtk.IconSizeMenu
    step <- Gtk.imageMenuItemNewWithLabel ""
    Gtk.imageMenuItemSetImage step image
    Gtk.menuShellAppend toolBar step

    -- create run button
    imageD <- Gtk.imageNewFromStock Gtk.stockGoForward Gtk.IconSizeMenu
    run <- Gtk.imageMenuItemNewWithLabel ""
    Gtk.imageMenuItemSetImage run imageD
    Gtk.menuShellAppend toolBar run

    variables <- Gtk.menuItemNewWithLabel "variables"

    reset <- Gtk.menuItemNewWithLabel "reset"

    -- create mode-menu
    mode <- Gtk.menuNew

    -- create modes
    insertMode <- Gtk.radioMenuItemNewWithLabel "insert"
    replaceMode <- Gtk.radioMenuItemNewWithLabelFromWidget insertMode "replace"
    smartMode <- Gtk.radioMenuItemNewWithLabelFromWidget insertMode "smart"

    highlightCheck <- Gtk.checkMenuItemNewWithLabel "highlighting"
    Gtk.checkMenuItemSetActive highlightCheck True
    let dataBuffer = IDF.getDataStackBuffer interDT
    let funcBuffer = IDF.getFunctionStackBuffer interDT

    Gtk.onButtonPress run  $ \event -> do
      tac <- readIORef $ TA.textAreaContent area
      intCtxt <- readIORef $ TAC.context tac
      IN.interpret tac
      intCtxt <- readIORef $ TAC.context tac
      TA.redrawContent area
      Gtk.textBufferSetText dataBuffer $ unlines $ map show $ TAC.dataStack intCtxt
      Gtk.textBufferSetText funcBuffer $ unlines $ map (\(x,_,_)->x) $ TAC.funcStack intCtxt
      let fS = TAC.funcStack intCtxt
      if ((length fS) > 0) 
      then do
        let ip = (\(_,x,_) -> (Lexer.posx x,Lexer.posy x)) $ head fS
        putStrLn $ show ip
      else do
        let ip = (0,0)
        putStrLn $ show ip
      return True

    Gtk.onButtonPress step $ \event -> do
      tac <- readIORef $ TA.textAreaContent area
      intCtxt <- readIORef $ TAC.context tac
      IN.step tac
      intCtxt <- readIORef $ TAC.context tac
      TA.redrawContent area
      Gtk.textBufferSetText dataBuffer $ unlines $ map show $ TAC.dataStack intCtxt
      Gtk.textBufferSetText funcBuffer $ unlines $ map (\(x,_,_)->x) $ TAC.funcStack intCtxt
      let fS = TAC.funcStack intCtxt
      if ((length fS) > 0) 
      then do
        let ip = (\(_,x,_) -> (Lexer.posx x,Lexer.posy x)) $ head fS
        putStrLn $ show ip
      else do
        let ip = (0,0)
        putStrLn $ show ip
      return True

    bufferVariables <- Gtk.textBufferNew Nothing

    Gtk.onButtonPress variables $ \event -> do
      tac <- readIORef $ TA.textAreaContent area
      intCtxt <- readIORef $ TAC.context tac
      let list = TAC.funcStack intCtxt
      when (length list /= 0) $ do
        let vars = unlines $ map (\(x,y)-> x ++ " = " ++ (show y)) $ Map.toList $ (\(_,_,x) -> x)$ head $ list
        Gtk.textBufferSetText bufferVariables vars
        Gtk.postGUIAsync $ IDF.textViewWindowShow bufferVariables "Variables"
      return True

    Gtk.onButtonPress reset $ \event-> do
      tac <- readIORef $ TA.textAreaContent area
      Gtk.textBufferSetText bufferVariables ""
      Gtk.textBufferSetText dataBuffer ""
      Gtk.textBufferSetText funcBuffer ""
      IN.reset tac
      TA.redrawContent area
      return True

    -- set mode action
    Gtk.on insertMode Gtk.menuItemActivate$ do
      TA.setInputMode area KH.Insert
      FB.setMode footer KH.Insert

    Gtk.on replaceMode Gtk.menuItemActivate $ do
      TA.setInputMode area KH.Replace
      FB.setMode footer KH.Replace

    Gtk.on smartMode Gtk.menuItemActivate$ do
      TA.setInputMode area KH.Smart
      FB.setMode footer KH.Smart

    Gtk.on highlightCheck Gtk.menuItemActivate$ do
      isActive <- Gtk.checkMenuItemGetActive highlightCheck
      if isActive
      then TA.setHighlighting area True
      else TA.setHighlighting area False

    -- configure mode-menu
    modeItem <- Gtk.menuItemNewWithLabel "mode"
    Gtk.menuItemSetSubmenu modeItem mode

    Gtk.menuShellAppend toolBar modeItem

    Gtk.menuShellAppend mode insertMode
    Gtk.menuShellAppend mode replaceMode
    Gtk.menuShellAppend mode smartMode
    Gtk.menuShellAppend mode highlightCheck
    Gtk.menuShellAppend toolBar variables
    Gtk.menuShellAppend toolBar reset

    return toolBar
