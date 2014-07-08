{- |
Module      :  InteractionField.hs
Description :  .
Maintainer  :  Kelvin Glaß, Chritoph Graebnitz, Kristin Knorr, Nicolas Lehmann (c)
License     :  MIT

Stability   :  experimental

The InteractionField-module depicts the interaction-field on the right side of the main-window that contains the input, output, function-stack, variable-stack.
-}
module InteractionField (
-- *Constructors
  create,
-- *Types
  InteractionDT,
-- *Methods
  getContainer,
  getInputBuffer,
  getOutputBuffer
                        )
  where

    -- imports --

import qualified Graphics.UI.Gtk as Gtk
    -- functions --

data InteractionDT = InterDT { getContainer :: Gtk.VBox, getInputBuffer :: Gtk.TextBuffer, getOutputBuffer :: Gtk.TextBuffer}

create = do
  bufferIn <- Gtk.textBufferNew Nothing
  bufferOut <- Gtk.textBufferNew Nothing
  bufferStackFunc <- Gtk.textBufferNew Nothing
  bufferStackVar <- Gtk.textBufferNew Nothing

  labelIn <- Gtk.labelNewWithMnemonic "Input:"
  viewIn <- Gtk.textViewNewWithBuffer bufferIn
  labelOut <- Gtk.labelNewWithMnemonic "Output:"
  viewOut <- Gtk.textViewNewWithBuffer bufferOut
  labelStackFunc <- Gtk.labelNewWithMnemonic "Functionstack"
  viewStackFunc <- Gtk.textViewNewWithBuffer bufferStackFunc
  labelStackVar <- Gtk.labelNewWithMnemonic "Variablestack"
  viewStackVar <- Gtk.textViewNewWithBuffer bufferStackVar

  buttonPopUpIn <- Gtk.buttonNewWithLabel ""
  setButtonProps buttonPopUpIn
  buttonPopUpOut <- Gtk.buttonNewWithLabel ""
  setButtonProps buttonPopUpOut
  buttonPopUpStackF <- Gtk.buttonNewWithLabel ""
  setButtonProps buttonPopUpStackF
  buttonPopUpStackV <- Gtk.buttonNewWithLabel ""
  setButtonProps buttonPopUpStackV

  Gtk.onClicked buttonPopUpIn $ Gtk.postGUIAsync $ textViewWindowShow bufferIn "Input"
  Gtk.onClicked buttonPopUpOut $ Gtk.postGUIAsync $ textViewWindowShow bufferOut "Output"
  Gtk.onClicked buttonPopUpStackF $ Gtk.postGUIAsync $ textViewWindowShow bufferStackFunc "Function-Stack"
  Gtk.onClicked buttonPopUpStackV $ Gtk.postGUIAsync $ textViewWindowShow bufferStackVar "Variable-Stack"

  hboxLabelButtonIn <- Gtk.hBoxNew False 0
  Gtk.boxPackStart hboxLabelButtonIn labelIn Gtk.PackNatural 1
  Gtk.boxPackEnd hboxLabelButtonIn buttonPopUpIn Gtk.PackNatural 0

  hboxLabelButtonOut <- Gtk.hBoxNew False 0
  Gtk.boxPackStart hboxLabelButtonOut labelOut Gtk.PackNatural 1
  Gtk.boxPackEnd hboxLabelButtonOut buttonPopUpOut Gtk.PackNatural 0

  boxStackFunc <- Gtk.vBoxNew False 0
  hboxLabelButtonFunc <- Gtk.hBoxNew False 0
  Gtk.boxPackStart hboxLabelButtonFunc labelStackFunc Gtk.PackNatural 0
  Gtk.boxPackEnd hboxLabelButtonFunc buttonPopUpStackF Gtk.PackNatural 10
  Gtk.boxPackStart boxStackFunc hboxLabelButtonFunc Gtk.PackNatural 0
  swinStackF  <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.scrolledWindowSetPolicy swinStackF Gtk.PolicyAutomatic Gtk.PolicyAutomatic
  Gtk.containerAdd swinStackF viewStackFunc
  Gtk.boxPackStart boxStackFunc swinStackF Gtk.PackGrow 0

  boxStackVar <- Gtk.vBoxNew False 0
  hboxLabelButtonVar <- Gtk.hBoxNew False 0
  Gtk.boxPackStart hboxLabelButtonVar labelStackVar Gtk.PackNatural 0
  Gtk.boxPackEnd hboxLabelButtonVar buttonPopUpStackV Gtk.PackNatural 10
  Gtk.boxPackStart boxStackVar hboxLabelButtonVar Gtk.PackNatural 0
  swinStackV <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.scrolledWindowSetPolicy swinStackV Gtk.PolicyAutomatic Gtk.PolicyAutomatic
  Gtk.containerAdd swinStackV viewStackVar
  Gtk.boxPackStart boxStackVar swinStackV Gtk.PackGrow 0

  boxStack <- Gtk.hBoxNew True 0
  Gtk.boxPackStart boxStack boxStackFunc Gtk.PackGrow 2
  Gtk.boxPackStart boxStack boxStackVar Gtk.PackGrow 2

  boxView <- Gtk.vBoxNew False 0

  Gtk.boxPackStart boxView hboxLabelButtonIn Gtk.PackNatural 2
  swinIn <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.scrolledWindowSetPolicy swinIn Gtk.PolicyAutomatic Gtk.PolicyAutomatic
  Gtk.containerAdd swinIn viewIn
  Gtk.boxPackStart boxView swinIn Gtk.PackGrow 0
  inSap <- Gtk.hSeparatorNew
  Gtk.boxPackStart boxView inSap Gtk.PackNatural 2
  Gtk.boxPackStart boxView hboxLabelButtonOut Gtk.PackNatural 2
  swinOut <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.scrolledWindowSetPolicy swinOut Gtk.PolicyAutomatic Gtk.PolicyAutomatic
  Gtk.containerAdd swinOut viewOut
  Gtk.boxPackStart boxView swinOut Gtk.PackGrow 1
  outSap <- Gtk.hSeparatorNew
  Gtk.boxPackStart boxView outSap Gtk.PackNatural 2
  Gtk.boxPackStart boxView boxStack Gtk.PackGrow 1

  return $ InterDT boxView bufferIn bufferOut

textViewWindowShow textBuffer title = do
  window <- Gtk.windowNew
  Gtk.windowSetDefaultSize window 400 300
  Gtk.windowSetPosition window Gtk.WinPosCenter
  swin <- Gtk.scrolledWindowNew Nothing Nothing
  Gtk.scrolledWindowSetPolicy swin Gtk.PolicyAutomatic Gtk.PolicyAutomatic
  textView <- Gtk.textViewNewWithBuffer textBuffer
  Gtk.containerAdd swin textView
  Gtk.set window [Gtk.containerChild Gtk.:= swin,
                  Gtk.windowTitle Gtk.:= title]
  Gtk.widgetShowAll window
  return ()

setButtonProps button = do
  image <- Gtk.imageNewFromFile "full.png"
  Gtk.buttonSetImage button image
  Gtk.buttonSetImagePosition button Gtk.PosRight
