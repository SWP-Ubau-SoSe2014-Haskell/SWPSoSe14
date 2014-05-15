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
 
  
 -- functions --

 -- |Entrypoint of rail2llvm-compiler.
 main :: IO ()
 main = do exportAST "../rail-examples/hello-world.rail" "x.ast"
  where
   help = putStrLn "output help (TODO)"
   exportAST inputfile outputfile = do input <- readFile inputfile
                                       let content = Lexer.fromAST . Lexer.process . PreProc.process $ IIP input
                                       putStrLn content
