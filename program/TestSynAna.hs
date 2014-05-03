module TestSynAna (
                   testModule     -- tests the module SyntacticalAnalysis
                  )
 where

 -- imports --
 import Test.HUnit
 import InterfaceDT                   as IDT
 import qualified SyntacticalAnalysis as SynAna

 -- functions --
 -- |Hello World
 testSynAna01 = "SyntactiaclAnalysis: " ~: (output1) @=? (SynAna.process input1)
 input1  = ILS [("main", [(1,Start,2),(2, Constant "Hello World!", 3),(3, Output, 4),(4, Finish, 0)])]
 output1 = ISS [("main", [(1,[Start, Constant "Hello World!", Output, Finish],0)])]
 -- testSynAna02 = "SyntactiaclAnalysis: " ~: (erwarteter wert) @=? (SynAna.process eingabe)
 -- testSynAna03 = "SyntactiaclAnalysis: " ~: (erwarteter wert) @=? (SynAna.process eingabe)
 -- testSynAna04 = "SyntactiaclAnalysis: " ~: (erwarteter wert) @=? (SynAna.process eingabe)
 -- testSynAna05 = "SyntactiaclAnalysis: " ~: (erwarteter wert) @=? (SynAna.process eingabe)
 -- ...
 
 testModule = [] -- [testSynAna01,testSynAna02,testSynAna03,testSynAna04,testSynAna05]
