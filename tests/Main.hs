module Main(main) where

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

import System.Exit

-- returns an appropriate ExitCode
getExitCode :: Counts -> ExitCode
getExitCode Counts { errors = 0, failures = 0 } = ExitSuccess
getExitCode _ = ExitFailure 1

main :: IO ()
main = do
  counts <- runTestTT $ TestList (
    TestPreProc.testModule ++
    TestLexer.testModule ++
    TestSynAna.testModule ++
    TestSemAna.testModule ++
    TestInterCode.testModule ++
    TestCodeOpt.testModule ++
    TestBackend.testModule
    )
  exitWith $ getExitCode counts
