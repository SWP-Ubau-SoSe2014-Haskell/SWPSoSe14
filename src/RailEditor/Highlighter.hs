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
import qualified Data.Map as Map

--returns the grid2D from a IDT.IPL grid2D
getGrid2dFromPreProc2Lexer(IDT.IPL grid2D) = grid2D

-- highlights all entries saved in the data structur of the TextAreaCotent-module
highlight :: TAC.TextAreaContent -> IO()
highlight textAC =
  EXC.catch (do
    pGrid <- TAC.getPositionedGrid textAC
    let (IDT.IPL positionedGrid) = pGrid
    (xm,ym) <- TAC.size textAC
    paintItRed textAC
    highlightFcts positionedGrid textAC
    return ()
    ) handleErrors

--Handels errors and prints them out used in context of Lexer and Preprocessor
handleErrors :: EXC.ErrorCall  -> IO ()
handleErrors e = print(show e)

-- highlight all rail-functions
highlightFcts ::  [PositionedGrid]-- List of funtions in line-representation with y coord of function(position of $) 
  -> TAC.TextAreaContent             -- Char coloring information
  -> IO IP
highlightFcts [] _ = return crash
highlightFcts (x:xs) textAC = do
  highlightFct (fst x) start (snd x) textAC (Map.empty)
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
  -> Map.Map (Int,Int) Bool --Map of colored positions
  -> IO IP
highlightFct grid2D ip yOffset textAC mOCPos
  | ip == crash = return crash
  |otherwise =
  if isPosColored mOCPos (posx ip,(posy ip))
  then return crash
  else do
   case lex of
    Nothing -> do
      TAC.putColor textAC (xC,yC) TAC.black
      highlightFct grid2D nextIP yOffset textAC inMap
    Just (Junction _) -> do
      TAC.putColor textAC (xC,yC) TAC.gold
      let (falseIP,trueIP) = junctionturns grid2D parseIP
      highlightFct grid2D falseIP yOffset textAC inMap
      highlightFct grid2D trueIP yOffset textAC inMap
    Just (Lambda _) -> do
      TAC.putColor textAC (xC,yC) TAC.gold
      let (lip,bip) = lambdadirs parseIP
      highlightFct grid2D (step grid2D lip) yOffset textAC inMap
      highlightFct grid2D (step grid2D bip) yOffset textAC inMap
    Just (Constant str)   ->
      if [current grid2D parseIP] == "]" || 
         [current grid2D parseIP] == "["
      then colorStrCommand str TAC.green
      else do
        TAC.putColor textAC (xC,yC) TAC.green
        highlightFct grid2D (step grid2D parseIP)yOffset textAC inMap
    Just (Push str)-> colorStrCommand str TAC.blue
    Just (Pop str) -> colorStrCommand str TAC.green
    Just (Call str) -> colorStrCommand str TAC.green
    _ -> do
      cBlue
      cGold
      if lex == Just Finish
      then return crash
      else highlightFct grid2D nextIP yOffset textAC inMap
    where
      (lex, parseIP) = parse grid2D ip
      nextIP = step grid2D parseIP
      x = posx ip
      y = posy ip
      xC = fromIntegral $ x
      yC = fromIntegral $ y+yOffset
      inMap = Map.insert (x,y) True mOCPos
      -- colors Start and finish gold
      cGold ::IO ()
      cGold | fromJust lex `elem` [Start,Finish] = TAC.putColor textAC (xC,yC) TAC.gold
            | otherwise = return()
      -- colors rail-builtins blue
      cBlue :: IO ()
      cBlue | fromJust lex `elem` [NOP,Boom,EOF,Input,Output,IDT.Underflow,
              RType,Add1,Divide,Multiply,Subtract,Remainder,Cut,Append,Size,Nil,
              Cons,Breakup,Greater,Equal] = TAC.putColor textAC (xC,yC) TAC.blue
            | otherwise = return()
      --function to color commands with strings like [], {}
      colorStrCommand :: String -> TAC.RGBColor -> IO IP
      colorStrCommand str color = do
        colorMoves grid2D (turnaround ip)
          (turnaround parseIP) color textAC
        highlightFct grid2D (step grid2D parseIP) yOffset textAC inMap
      --steps the IP to the beginning of an constant, call or pop
      colorMoves :: Grid2D -> IP -> IP -> TAC.RGBColor -> TAC.TextAreaContent-> IO IP
      colorMoves grid2D endIP curIP color textAC 
        | endIP == curIP = do
          TAC.putColor textAC (x,y) color
          return crash
        | otherwise = do
          TAC.putColor textAC (x,y) color
          colorMoves grid2D endIP (move curIP Forward) color textAC
          return crash
        where
          x = fromIntegral $ posx curIP
          y = fromIntegral $ posy curIP+yOffset
          
-- colors all entry red
-- This function is needed to recolor after editing
paintItRed :: TAC.TextAreaContent -> IO ()
paintItRed = TAC.deleteColors
  
-- Is the position colored?
isPosColored :: Map.Map (Int,Int) Bool
  -> (Int,Int)
  -> Bool
isPosColored mOCPos pos =
  isJust $ Map.lookup pos mOCPos
  
