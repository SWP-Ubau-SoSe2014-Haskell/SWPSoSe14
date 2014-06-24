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
import Preprocessor as PRE
import Lexer
import qualified TextAreaContent as TAC
import qualified Control.Exception as EXC
import Graphics.UI.Gtk.Abstract.Widget
import System.IO
import Data.IORef
import Data.Maybe

--returns the grid2D from a IDT.IPL grid2D
getGrid2dFromPreProc2Lexer(IDT.IPL grid2D) = grid2D

-- highlights all entries saved in the data structur of the TextAreaCotent-module
highlight :: TAC.TextAreaContent -> IO()
highlight textAC = do
  code <- TAC.serialize textAC
  EXC.catch (do
    let positionedGrid =  getGrid2dFromPreProc2Lexer $ PRE.process  (IIP code)
    (xm,ym) <- TAC.size textAC
    paintItRed 0 0 xm ym textAC --TODO Do it efficient using redo/undo to get the current pressed entry
    highlightFcts positionedGrid textAC
    return ()
    ) handleErrors

--Handels errors and prints them out used in context of Lexer and Preprocessor
handleErrors :: EXC.ErrorCall  -> IO ()
handleErrors e = print(show e)
{-
  | excep == Nothing = return ()
  | otherwise = putStrLn $ fromJust excep
  where
    excep = EXC.fromException e-}

-- highlight all rail-functions
highlightFcts ::  [PositionedGrid]-- List of funtions in line-representation with y coord of function(position of $) 
  -> TAC.TextAreaContent             -- Char coloring information
  -> IO IP
highlightFcts [] _ = return crash
highlightFcts (x:xs) textAC = do
  highlightFct (fst x) start (snd x) textAC
  highlightFcts xs textAC
  
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
highlightFct :: Grid2D
  -> IP
  -> Int
  -> TAC.TextAreaContent
  -> IO IP
highlightFct grid2D ip yOffset textAC
  | ip == crash = return crash
  |otherwise =
  case lex of
    Nothing -> do
      TAC.putColor textAC (xC,yC) TAC.black
      highlightFct grid2D nextIP yOffset textAC
    Just (Junction _) -> do
      TAC.putColor textAC (xC,yC) TAC.gold
      let (falseIP,trueIP) = junctionturns grid2D parseIP
      highlightFct grid2D falseIP yOffset textAC
      highlightFct grid2D trueIP yOffset textAC
    Just (Constant str)   ->
      if [current grid2D parseIP] == "]" || 
         [current grid2D parseIP] == "["
      then colorStrCommand str TAC.green
      else do
        TAC.putColor textAC (xC,yC) TAC.green
        highlightFct grid2D (step grid2D parseIP)yOffset textAC
    Just (Push str)-> colorStrCommand str TAC.blue
    Just (Pop str) -> colorStrCommand str TAC.green
    Just (Call str) -> colorStrCommand str TAC.green
    _ -> do
      cBlue
      highlightFct grid2D nextIP yOffset textAC
    where
      (lex, parseIP) = parse grid2D ip
      nextIP = step grid2D parseIP
      xC = posx ip
      yC = posy ip+yOffset
      -- colors rail-builtins blue
      cBlue :: IO ()
      cBlue | fromJust lex `elem` [NOP,Boom,EOF,Input,Output,IDT.Underflow,
              RType,Add1,Divide,Multiply,Subtract,Remainder,Cut,Append,Size,Nil,
              Cons,Breakup,Greater,Equal] = TAC.putColor textAC (xC,yC) TAC.blue
            |otherwise = return()
      --function to color commands with strings like [], {}
      colorStrCommand :: String -> Color -> IO IP
      colorStrCommand str color = do
        colorMoves grid2D (turnaround ip)
          (turnaround parseIP) color textAC
        highlightFct grid2D (step grid2D parseIP) yOffset textAC
      --steps the IP to the beginning of an constant, call or pop
      colorMoves :: Grid2D -> IP -> IP -> Color -> TAC.TextAreaContent-> IO IP
      colorMoves grid2D endIP curIP color textAC 
        | endIP == curIP = do
          TAC.putColor textAC (posx curIP,posy curIP+yOffset) color
          return crash
        | otherwise = do
          TAC.putColor textAC (posx curIP,posy curIP+yOffset) color
          colorMoves grid2D endIP (move curIP Forward) color textAC
          return crash

-- colors all entry red in a rect from x,y to xMax,yMax
-- This function is needed to recolor after editing
paintItRed :: Int-- x coord start
  -> Int--y coord str
  -> Int--x coord end
  -> Int--y coord end
  -> TAC.TextAreaContent
  -> IO [IO()]
paintItRed x y xMax yMax textAC =
  return [TAC.putColor textAC (xs,ys) TAC.red | xs <- [x..xMax], ys <- [y..yMax]]
  
  
  
