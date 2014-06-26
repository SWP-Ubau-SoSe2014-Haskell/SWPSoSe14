module EditorTypes where

import Graphics.UI.Gtk
import Data.IORef
import Data.Map as Map

defaultFgColor :: Color
defaultFgColor = Color 0 0 0 

defaultCursorColor :: Color
defaultCursorColor = Color 65535 0 0 

defaultBgColor :: Color
defaultBgColor = Color 65535 65535 65535

defaultRowColumSelectColor :: Color
defaultRowColumSelectColor = Color 60000 60000 60000

data RGBColor = RGBColor Double Double Double
data ContentEntry = ContentEntry Position Char

type Position = (Double,Double) 
type Content = IORef (Map.Map Position Char)
type CurrentPosition = IORef Position
type ContentList = [(Position,Char)]

black = RGBColor 0.0 1.0 0.0
white = 1.0

width = 600.0
height = 400.0
bef = 15.0
hef = 16.0
