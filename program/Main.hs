{- |
Module      :  Main.hs
Description :  .
Copyright   :  (c) Christopher Pockrandt, Nicolas Lehmann
License     :  MIT

Stability   :  experimental

Entrypoint of the rail2llvm-compiler. Contains main-function.

Commands:
 help:          SWPSoSe2014 --help
 compile:       SWPSoSe2014 --compile   <inputfile> <outputfile>
 export AST:    SWPSoSe2014 --exportAST <inputfile> <outputfile>
 import AST:    SWPSoSe2014 --importAST <inputfile> <outputfile>

-}
module Main (
             main  -- main-function to run the program
            )
where

 -- imports --
 import System.Environment
 import InterfaceDT                   as IDT
 import qualified Preprocessor        as PreProc
 import qualified Lexer
 import qualified SyntacticalAnalysis as SynAna
 import qualified SemanticalAnalysis  as SemAna
 import qualified IntermediateCode    as InterCode
 import qualified CodeOptimization    as CodeOpt
 import qualified Backend
 
  
 -- functions --

 -- |Entrypoint of rail2llvm-compiler.
 main :: IO ()
 main = do args <- getArgs
           case args of
                "--compile":inputfile:outputfile:[]   -> compile inputfile outputfile
                "--exportAST":inputfile:outputfile:[] -> exportAST inputfile outputfile
                "--importAST":inputfile:outputfile:[] -> importAST inputfile outputfile
                _                                     -> help   
  where
   help = putStrLn "output help (TODO)"
   compile inputfile outputfile = do input <- readFile inputfile
                                     let output (IBO x) = x
                                     content <- output $ Backend.process . CodeOpt.process . InterCode.process . SemAna.process . SynAna.process . Lexer.process . PreProc.process $ IIP input
                                     writeFile outputfile content

   importAST inputfile outputfile = do input <- readFile inputfile
                                       let output (IBO x) = x
                                       content <- output $ Backend.process . CodeOpt.process . InterCode.process . SemAna.process . SynAna.process $ map toGraph $ splitfunctions input
                                       writeFile outputfile content

   exportAST inputfile outputfile = do input <- readFile inputfile
                                       content <- map fromGraph $ Lexer.process . PreProc.process $ IIP input
                                       writeFile outputfile content
   
fromGraph :: IDT.Graph -> String
fromGraph (funcname, nodes) = unlines $ ("["++funcname++"]"):(map (Lexer.offset (-1)) $ tail $ map fromLexNode nodes)
 where
  fromLexNode :: IDT.LexNode -> String
  fromLexNode (id, lexeme, follower) = (show id)++";"++(fromLexeme lexeme)++";"++(show follower)++(optional lexeme)
  fromLexeme :: IDT.Lexeme -> String
  fromLexeme Boom = "b"
  fromLexeme EOF = "e"
  fromLexeme Input = "i"
  fromLexeme Output = "o"
  fromLexeme Underflow = "u"
  fromLexeme RType = "?"
  fromLexeme (Constant string) = "["++string++"]"
  fromLexeme (Push string) = "("++string++")"
  fromLexeme (Pop string) = "(!"++string++"!)"
  fromLexeme (Call string) = "{"++string++"}"
  fromLexeme Add = "a"
  fromLexeme Divide = "d"
  fromLexeme Multiply = "m"
  fromLexeme Remainder = "r"
  fromLexeme Substract = "s"
  fromLexeme Cut = "c"
  fromLexeme Append = "p"
  fromLexeme Size = "z"
  fromLexeme Nil = "n"
  fromLexeme Cons = ":"
  fromLexeme Breakup = "~"
  fromLexeme Greater = "g"
  fromLexeme Equal = "q"
  fromLexeme Start = "$"
  fromLexeme Finish = "#"
  fromLexeme (Junction _) = "v"
  optional (Junction follow) = ","++(show follow)
  optional _ = ""

splitfunctions :: String -> [String]
splitfunctions "" = [""]
splitfunctions code = (unlines (ln:fun)):(splitfunctions other)
 where
  (ln:lns) = lines code
  (fun, other) = span (\x -> (head x) /= '[')

toGraph :: String -> IDT.Graph
toGraph string = (init $ tail $ head lns, (1, Start, 2):(map (Lexer.offset 1) $ nodes $ tail lns))
 where
  lns = lines string
  nodes [] = []
  nodes (ln:lns) = (read id, fixedlex, read follower):(nodes lns)
   where
    (id, other) = span (/=';') ln
    (lex, ip) = Lexer.parse [other] $ Lexer.IP 1 0 Lexer.E
    fixedlex
     | other!!2 `elem` "v^<>" = Junction (read $ tail $ dropWhile (/=',') other)
     | otherwise = fromJust lex
    fromJust Nothing = error ("line with no lexem found in line: "++ln)
    fromJust (Just x) = x
    follower = takeWhile (/=',') $ dropWhile (\x -> not (x `elem` "0123456789")) $ drop (Lexer.posx ip) other
