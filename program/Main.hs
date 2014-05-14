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
                                       content <- output $ Backend.process . CodeOpt.process . InterCode.process . SemAna.process . SynAna.process . Lexer.toAST input
                                       writeFile outputfile content

   exportAST inputfile outputfile = do input <- readFile inputfile
                                       content <- Lexer.fromAST . Lexer.process . PreProc.process $ IIP input
                                       writeFile outputfile content
   
