module Testing (
                testModules     -- tests all submodules
               )
 where

 -- imports --
 import Test.HUnit
 import InterfaceDT                   as IDT
 import qualified Preprocessor        as PreProc
 import qualified Lexer
 import qualified SyntacticalAnalysis as SynAna
 import qualified SemanticalAnalysis  as SemAna
 import qualified IntermediateCode    as InterCode
 import qualified CodeOptimization    as CodeOpt
 import qualified Backend

 -- functions --
 --testPreProc   = "PreProcessor: " ~: (erwarteter wert) @=? (PreProc.process eingabe)
 --testLexer     = "Lexer: " ~: (erwarteter wert) @=? (Lexer.process eingabe)
 --testSynAna    = "SyntactiaclAnalysis: " ~: (erwarteter wert) @=? (SynAna.process eingabe)
 --testSemAna    = "SemanticalAnalysis: " ~: (erwarteter wert) @=? (SemAna.process eingabe)
 --testInterCode = "IntermediateCode: " ~: (erwarteter wert) @=? (InterCode.process eingabe)
 --testCodeOpt   = "CodeOptimization: " ~: (erwarteter wert) @=? (CodeOpt.process eingabe)
 --testBackend   = "Backend: " ~: (erwarteter wert) @=? (Backend.process eingabe)
 
 testModules = undefined --runTestTT $ TestList [testPreProc,       -- tests the preprocessor module
                         --            testLexer,         -- tests the lexer module
                         --            testSynAna,        -- tests the syntactical analysis module
                         --            testSemAna,        -- tests the semantical analysis module
                         --            testInterCode,     -- tests the intermadiate code module
                         --            testCodeOpt,       -- tests the code optimization mode
                         --            testBackend]       -- tests the backend module