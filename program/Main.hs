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
 main = undefined  -- (todo...)

 -- 1. readfile               <- Readfile "inputfile"
 -- 2. resultOfPreProc        <- PreProc.process readfile
 -- 3. resultOfLexer          <- Lexer.process resultOfPreProc
 -- 4. resultOfSynAna         <- SynAna.process resultOfLexer
 -- 5. resultOfSemAna         <- SemAna.process resultOfSynAna
 -- 6. resultOfInterCode      <- InterCode resultOfSemAna
 -- 7. resultOfCodeOpt        <- CodeOpt.process resultOfInterCode
 -- 8. resultOfBackend        <- Backend.process resultOfCodeOpt
 -- 9. Outputfile             <- writefile "resultOfBackend" 