module Testing (
                testModules     -- tests all submodules
               )
 where

 -- imports --
 import Test.HUnit
 import InterfaceDT                   as IDT
 import qualified TestPreProc
 import qualified TestLexer
 import qualified TestSynAna
 import qualified TestSemAna
 import qualified TestInterCode
 import qualified TestCodeOpt
 import qualified TestBackend

 testModules = runTestTT $ TestList (TestPreProc.testModule++TestLexer.testModule++TestSynAna.testModule++TestSemAna.testModule++TestInterCode.testModule++TestCodeOpt.testModule++TestBackend.testModule)