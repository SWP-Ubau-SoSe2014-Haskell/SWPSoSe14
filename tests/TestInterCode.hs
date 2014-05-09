module TestInterCode (testModule) where

 -- imports --
import Test.HUnit
import InterfaceDT                   as IDT
import qualified IntermediateCode    as InterCode

-- exmaple test function --
-- testInterCode01 = "IntermediateCode: " ~: (erwarteter wert) @=? (InterCode.process eingabe)

inputTestCase01 = ISI [("main", [(1,[Start, Constant "Hello World!", Output, Finish],0)])]

testInterCode01 = "IntermediateCode: " ~:
  InterCode.process inputTestCase01 @=? InterCode.process inputTestCase01

testModule = [ testInterCode01 ]
