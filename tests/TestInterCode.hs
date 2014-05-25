module TestInterCode (testModule) where

 -- imports --
import Test.HUnit
import InterfaceDT                   as IDT
import qualified IntermediateCode    as InterCode

-- exmaple test function --
-- testInterCode01 = "IntermediateCode: " ~: (expected value) @=? (InterCode.process input)

-- working "Hello World" program
inputTestCase01 = ISI [("main", [(1,[Start, Constant "Hello World!", Output, Finish],0)])]

-- empty path
input2 = ISI []
-- points to non-existent path
input3 = ISI [("main", [(1,[Start, Finish],99)])]
-- wrong starting ID
input4 = ISI [("main", [(99,[Start, Finish],0)])]
-- empty string as function name
input5 = ISI [("", [(1,[Start, Finish],0)])]

-- incorrect output to produce failures and be able to view the actual output of the module
testInterCode01 = "IntermediateCode: " ~:
  InterCode.process inputTestCase01 @=? InterCode.process inputTestCase01

testInterCode02 = "IntermediateCode: " ~:
  InterCode.process inputTestCase01 @=? InterCode.process input2

testInterCode03 = "IntermediateCode: " ~:
  InterCode.process inputTestCase01 @=? InterCode.process input3

testInterCode04 = "IntermediateCode: " ~:
  InterCode.process inputTestCase01 @=? InterCode.process input4

testInterCode05 = "IntermediateCode: " ~:
  InterCode.process inputTestCase01 @=? InterCode.process input5

testModule = TestList[ testInterCode01, testInterCode02, testInterCode03, testInterCode04, testInterCode05 ]

run = runTestTT $ testModule