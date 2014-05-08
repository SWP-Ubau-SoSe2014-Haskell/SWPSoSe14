module Main (
             main  -- mainfunction to run the program
            )
where

-- imports --
import InterfaceDT                   as IDT
import qualified Testing             as Test
import qualified Preprocessor        as PreProc
import qualified Lexer
import qualified SyntacticalAnalysis as SynAna
import qualified SemanticalAnalysis  as SemAna
import qualified IntermediateCode    as InterCode
import qualified CodeOptimization    as CodeOpt
import qualified Backend


-- functions --
main :: IO()
main = do putStrLn "Enter inputfile (path): "
          inputfile <- getLine
          putStrLn "Enter outputfile (path): "
          outputfile <- getLine
          putStrLn ("Compiling " ++ inputfile ++ " to " ++ outputfile)
          do input <- readFile inputfile
             let output (IBO x) = x
             writeFile outputfile (output ((Backend.process . CodeOpt.process . InterCode.process . SemAna.process . SynAna.process . Lexer.process . PreProc.process) (IIP input)))
