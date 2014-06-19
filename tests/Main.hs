module Main(main) where

-- imports --
import Test.HUnit
import InterfaceDT                   as IDT
import qualified TPreProc
import qualified TLexer
import qualified TSynAna
import qualified TSemAna
import qualified TInterCode
import qualified TCodeOpt
import qualified TBackend

import System.Exit
import System.Process

-- returns an appropriate ExitCode
getExitCode :: Counts -> ExitCode
getExitCode Counts { errors = 0, failures = 0 } = ExitSuccess
getExitCode _ = ExitFailure 1

main :: IO ()
main = do
  counts <- runTestTT $ TestList (
    TPreProc.testModule ++
    TLexer.testModule ++
    TSynAna.testModule ++
    TSemAna.testModule ++
    TInterCode.testModule ++
    TCodeOpt.testModule ++
    TBackend.testModule
    )
  testexit <- system "tests/integration_tests"
  exitWith $ addexits testexit $ getExitCode counts

addexits :: ExitCode -> ExitCode -> ExitCode
addexits ExitSuccess ExitSuccess = ExitSuccess
addexits _ (ExitFailure _) = ExitFailure 1
addexits (ExitFailure _) _ = ExitFailure 1
