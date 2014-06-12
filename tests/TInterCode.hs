module TInterCode (testModule) where

 -- imports --
import Test.HUnit
import InterfaceDT                   as IDT
import qualified IntermediateCode    as InterCode

-- exmaple test function --
-- testInterCode01 = "IntermediateCode: " ~: (expected value) @=? (InterCode.process input)

-- working "Hello World" program
input01 = ISI [("main", [(1,[Start, Constant "Hello World!", Output, Finish],0)])]

-- NEGATIVE

-- empty path
input02 = ISI []
-- points to non-existent path
input03 = ISI [("main", [(1,[Start, Finish],99)])]
-- wrong starting ID
input04 = ISI [("main", [(99,[Start, Finish],0)])]
-- empty string as function name
input05 = ISI [("", [(1,[Start, Finish],0)])]
-- circle: 1 > 2 > 1
input06 = ISI [("main", [(1,[Start, Finish],2)]),("foo", [(2,[Start, Finish],1)])]

-- POSITIVE

-- empty main function
input07 = ISI [("main", [(1,[Start, Finish],0)])]

-- outputs 
output = ISI []

-- incorrect output to produce failures and be able to view the actual output of the module
testInterCode01 = "IntermediateCode: " ~:
  InterCode.process input01 @=? InterCode.process input01

testInterCode02 = "IntermediateCode: " ~:
  InterCode.process output @=? InterCode.process input02

testInterCode03 = "IntermediateCode: " ~:
  InterCode.process output @=? InterCode.process input03

testInterCode04 = "IntermediateCode: " ~:
  InterCode.process output @=? InterCode.process input04

testInterCode05 = "IntermediateCode: " ~:
  InterCode.process output @=? InterCode.process input05

testInterCode06 = "IntermediateCode: " ~:
  InterCode.process output @=? InterCode.process input06

testInterCode07 = "IntermediateCode: " ~:
  InterCode.process output @=? InterCode.process input07

--testModule = []
testModule = [TestLabel "Hello World" testInterCode01, TestLabel "empty path"
  testInterCode02, TestLabel "non-existent path" testInterCode03, TestLabel "wrong start ID" 
  testInterCode04, TestLabel "empty function name" testInterCode05, TestLabel "circle" 
  testInterCode06, TestLabel "empty main" testInterCode07]
