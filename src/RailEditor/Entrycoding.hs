module Entrycoding where

import Graphics.UI.Gtk


mapTable:: (Table -> Int -> Int -> IO Table) -> Table -> IO()
mapTable func table = do
                 size <- tableGetSize table
                 mapTableH func table (0,0) size
                 return ()
                    where
                    mapTableH func table (x,y) (sizeX,sizeY)
                        |x==sizeX && y==sizeY = return table
                        |x<sizeX = do
                                   upTable <- func table x y
                                   mapTableH func upTable ((x+1),y) (sizeX,sizeY)
                                   return table
                        |x==sizeX && y<sizeY = do
                                               mapTableH func table (0,(y+1)) (sizeX,sizeY)
                                               return table


entryInsert:: Table -> Int -> Int -> IO Table
entryInsert table x y = do
                        entry <- entryNew
                        set entry [entryWidthChars := 1]
                        entrySetMaxLength entry 1
                        entrySetHasFrame entry False
                        tableAttach table entry x (x+1) y (y+1) [] [] 0 0
--                        widgetModifyText entry StateNormal (Color 65535 0 0)
                        return table

