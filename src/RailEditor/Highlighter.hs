{-
Module : Highlighter.hs
Description : .
Maintainer : Chritoph Graebnitz, Marcus Hoffmann(c)
License : MIT

Stability : experimental

This Modul provides a function to color the colorMap of textAreaContent.
It uses primary the Lexer modul to step the IP of the code and highlight
by the way.
-}
module Highlighter (
                    highlight   -- highlights all entries saved in the data structur of the TextAreaCotent-module
                   )
  where

import InterfaceDT as IDT
import Preprocessor as Pre
import Lexer
import qualified TextAreaContent as TAC
import qualified Control.Exception as Exc
import System.IO
import Data.IORef
import Data.Maybe

-- highlights all entries saved in the data structur of the TextAreaCotent-module
highlight :: TAC.TextAreaContent -> IO()
highlight (chars, colors) = do
  code <- serialize (chars, colors)
  Exc.catch (do
    let (grid2D,indexes) = getGrid2dFromPreProc2Lexer $ Pre.process  (IIP code)
    (xm,ym) <- readIORef size chars
    paintItRed 0 0 xm ym colors --TODO Do it efficient using redo/undo to get the current pressed entry
    highlightFcts grid2D indexes colors
    return ()
    ) handleErrors

--Handels errors and prints them out used in context of Lexer and Preprocessor
handleErrors :: Exc.ErrorCall -> IO ()
handleErrors e 
  | excep == Nothing = return ()
  | otherwise = putStrLn $ fromJust excep
  where
    excep = fromException e

-- highlight all rail-functions
highlightFcts ::  [Grid2D]-- List of funtions in line-representation  
  -> [Int]                -- start indexes of function(y coord of textArea) 
  -> ColorMap             -- Char coloring information
  -> IO IP
highlightFcts [] _ _ = return crash
highlightFcts _ [] _ = return crash
highlightFcts (x:xs) (y:ys) colors = do
  highlightFct x start y colors
  highlightFcts xs ys colors
  
  {-
 main highlighting process which highlights a single rail-function.
 Colors:
   comments : red
   $ # : gold
   rails : black
   built in function blue
   constans green
 parseIP returns the current lexeme and IP. In case of constants IP
 is at the closing char.
-}
--putColor :: TextAreaContent -> (Int,Int) -> Color -> IO ()
highlightFct :: Grid2D
  -> IP
  -> Int
  -> ColorMap
  -> IO IP
highlightFct _ crash _ _ = return crash
highlightFct grid2D ip yOffset colors = do
  case lex of
    Nothing -> putColor (emptyCharMap,colors) (xC,yC) TAC.black
    Just (Junction _) -> do
      putColor (emptyCharMap,colors) (xC,yC) TAC.gold
      let (falseIP,trueIP) = junctionturns grid2D parseIP
      highlightFct textArea grid2D falseIP yOffset colors
      highlightFct textArea grid2D trueIP yOffset colors
      return ()
    Just (Constant str)   -> do
      if [(current grid2D parseIP)] == "]" || 
         [(current grid2D parseIP)] == "["
      then colorStrCommand str TAC.green
      else do
        putColor (emptyCharMap,colors) (xC,yC) TAC.green
        highlightFct grid2D (step grid2D parseIP)yOffset colors
      return ()
    Just (Push str)-> colorStrCommand str TAC.blue
    Just (Pop str) -> colorStrCommand str TAC.green
    Just (Call str) -> colorStrCommand str TAC.green
    _ -> do
      cBlue
      highlightFct grid2D nextIP yOffset colors
    where
      (lex, parseIP) = parse grid2D ip
      nextIP = step grid2D parseIP
      xC = posx ip
      yC = posy ip+yOffset
      -- colors rail-builtins blue
      cBlue :: IO ()
      cBlue | elem (fromJust lex) [NOP,Boom,EOF,Input,Output,IDT.Underflow,
        RType,Add1,Divide,Multiply,Subtract,Remainder,Cut,Append,Size,Nil,
        Cons,Breakup,Greater,Equal] = putColor (emptyCharMap,colors) (xC,yC) TAC.blue
      --function to color commands with strings like [], {}
      colorStrCommand :: String -> Color -> IO ()
      colorStrCommand str color = do
        colorMoves grid2D (turnaround ip)
          (turnaround parseIP) color colors
        highlightFct grid2D (step grid2D parseIP) yOffset colors
        return ()
      --steps the IP to the beginning of an constant, call or pop
      colorMoves :: Grid2D -> IP -> IP -> Color -> IO IP
      colorMoves grid2D endIP curIP color  
        | endIP == curIP = putColor (emptyCharMap,colors) (posx curIP,posy curIP+yOffset) color
        | otherwise = do
          putColor (emptyCharMap,colors)(posx curIP,posy curIP+yOffset) color
          colorMoves grid2D endIP (move curIP Forward) color colors
          return crash

emptyCharMap = TAC.ChM((newIORef empty) newIORef((0,0)))

-- colors all entry red in a rect from x,y to xMax,yMax
-- This function is needed to recolor after editing
paintItRed :: Int-- x coord start
  -> Int--y coord str
  -> Int--x coord end
  -> Int--y coord end
  -> colors
  -> IO()
paintItRed x y xMax yMax colors = do
  xs <- [x..xmax]
  ys <- [y..ymax]
  putColor (emptyCharMap,colors) (xs,ys) TAC.red
