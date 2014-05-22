module Main where

import Graphics.UI.Gtk

main = do
    initGUI
    window <- windowNew
    layout <- layoutNew Nothing Nothing
    table <- tableNew 100 100 True
    -- create Entries
    entry00 <- entryNew
    entry01 <- entryNew
    entry02 <- entryNew
    entry03 <- entryNew
    entry04 <- entryNew
    entry10 <- entryNew
    entry11 <- entryNew
    entry12 <- entryNew
    entry13 <- entryNew
    entry14 <- entryNew
    entry20 <- entryNew
    entry21 <- entryNew
    entry22 <- entryNew
    entry23 <- entryNew
    entry24 <- entryNew
    entry30 <- entryNew
    entry31 <- entryNew
    entry32 <- entryNew
    entry33 <- entryNew
    entry34 <- entryNew
    --set entry properties
    set entry00 [entryWidthChars := 1]
    entrySetMaxLength entry00 1
    entrySetHasFrame entry00 False
    set entry01 [entryWidthChars := 1]
    entrySetMaxLength entry01 1
    entrySetHasFrame entry01 False
    set entry02 [entryWidthChars := 1]
    entrySetMaxLength entry02 1
    entrySetHasFrame entry02 False
    set entry03 [entryWidthChars := 1]
    entrySetMaxLength entry03 1
    entrySetHasFrame entry03 False
    set entry04 [entryWidthChars := 1]
    entrySetMaxLength entry04 1
    entrySetHasFrame entry04 False
    set entry10 [entryWidthChars := 1]
    entrySetMaxLength entry10 1
    entrySetHasFrame entry10 False
    set entry11 [entryWidthChars := 1]
    entrySetMaxLength entry11 1
    entrySetHasFrame entry11 False
    set entry12 [entryWidthChars := 1]
    entrySetMaxLength entry12 1
    entrySetHasFrame entry12 False
    set entry13 [entryWidthChars := 1]
    entrySetMaxLength entry13 1
    entrySetHasFrame entry13 False
    set entry14 [entryWidthChars := 1]
    entrySetMaxLength entry14 1
    entrySetHasFrame entry14 False
    set entry20 [entryWidthChars := 1]
    entrySetMaxLength entry20 1
    entrySetHasFrame entry20 False
    set entry21 [entryWidthChars := 1]
    entrySetMaxLength entry21 1
    entrySetHasFrame entry21 False
    set entry22 [entryWidthChars := 1]
    entrySetMaxLength entry22 1
    entrySetHasFrame entry22 False
    set entry23 [entryWidthChars := 1]
    entrySetMaxLength entry23 1
    entrySetHasFrame entry23 False
    set entry24 [entryWidthChars := 1]
    entrySetMaxLength entry24 1
    entrySetHasFrame entry24 False
    set entry30 [entryWidthChars := 1]
    entrySetMaxLength entry30 1
    entrySetHasFrame entry30 False
    set entry31 [entryWidthChars := 1]
    entrySetMaxLength entry31 1
    entrySetHasFrame entry31 False
    set entry32 [entryWidthChars := 1]
    entrySetMaxLength entry32 1
    entrySetHasFrame entry32 False
    set entry33 [entryWidthChars := 1]
    entrySetMaxLength entry33 1
    entrySetHasFrame entry33 False
    set entry34 [entryWidthChars := 1]
    entrySetMaxLength entry34 1
    entrySetHasFrame entry34 False
    -- add Entries to layout
    tableAttach table entry00 0 1 0 1 [] [] 0 0
    tableAttach table entry01 1 2 0 1 [] [] 0 0
    tableAttach table entry02 2 3 0 1 [] [] 0 0
    tableAttach table entry03 3 4 0 1 [] [] 0 0
    tableAttach table entry04 4 5 0 1 [] [] 0 0
    tableAttach table entry10 0 1 1 2 [] [] 0 0
    tableAttach table entry11 1 2 1 2 [] [] 0 0
    tableAttach table entry12 2 3 1 2 [] [] 0 0
    tableAttach table entry13 3 4 1 2 [] [] 0 0
    tableAttach table entry14 4 5 1 2 [] [] 0 0
    tableAttach table entry20 0 1 2 3 [] [] 0 0
    tableAttach table entry21 1 2 2 3 [] [] 0 0
    tableAttach table entry22 2 3 2 3 [] [] 0 0
    tableAttach table entry23 3 4 2 3 [] [] 0 0
    tableAttach table entry24 4 5 2 3 [] [] 0 0
    tableAttach table entry30 0 1 3 4 [] [] 0 0
    tableAttach table entry31 1 2 3 4 [] [] 0 0
    tableAttach table entry32 2 3 3 4 [] [] 0 0
    tableAttach table entry33 3 4 3 4 [] [] 0 0
    tableAttach table entry34 4 5 3 4 [] [] 0 0
    -- change color for entries
    widgetModifyText entry00 StateNormal (Color 65535 0 0)
    widgetModifyText entry01 StateNormal (Color 65535 0 0)
    widgetModifyText entry02 StateNormal (Color 65535 0 0)
    widgetModifyText entry03 StateNormal (Color 65535 0 0)
    widgetModifyText entry04 StateNormal (Color 65535 0 0)
    widgetModifyText entry10 StateNormal (Color 0 65535 0)
    widgetModifyText entry11 StateNormal (Color 0 65535 0)
    widgetModifyText entry12 StateNormal (Color 0 65535 0)
    widgetModifyText entry13 StateNormal (Color 0 65535 0)
    widgetModifyText entry14 StateNormal (Color 0 65535 0)
    widgetModifyText entry20 StateNormal (Color 0 0 65535)
    widgetModifyText entry21 StateNormal (Color 0 0 65535)
    widgetModifyText entry22 StateNormal (Color 0 0 65535)
    widgetModifyText entry23 StateNormal (Color 0 0 65535)
    widgetModifyText entry24 StateNormal (Color 0 0 65535)
    widgetModifyText entry30 StateNormal (Color 65535 0 0)
    widgetModifyText entry31 StateNormal (Color 65535 0 0)
    widgetModifyText entry32 StateNormal (Color 65535 0 0)
    widgetModifyText entry33 StateNormal (Color 65535 0 0)
    widgetModifyText entry34 StateNormal (Color 65535 0 0)
    --add table layout to layout
    layoutPut layout table 0 0
    set window [ windowDefaultWidth  := 100,
                 windowDefaultHeight := 200,
                 containerChild      := layout ]
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
    return()

