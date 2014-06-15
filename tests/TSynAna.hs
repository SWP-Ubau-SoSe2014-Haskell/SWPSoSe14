module TSynAna (
                   testModule     -- tests the module SyntacticalAnalysis
                  )
 where

 -- imports --
 import Test.HUnit
 import InterfaceDT                   as IDT
 import qualified SyntacticalAnalysis as SynAna

 -- functions --
 testSynAna01 = "SyntactiaclAnalysis: " ~: output1 @=? SynAna.process input1
 testSynAna02 = "SyntactiaclAnalysis: " ~: output2 @=? SynAna.process input2
 testSynAna03 = "SyntactiaclAnalysis: " ~: output3 @=? SynAna.process input3
 testSynAna04 = "SyntactiaclAnalysis: " ~: output4 @=? SynAna.process input4
 
 input1  = ILS [("main", [(1,Start,2),(2, Constant "Hello World!", 3),(3, Output, 4),(4, Finish, 0)])]
 output1 = ISS [("main", [(1,[Start, Constant "Hello World!", Output, Finish],0)])]
 
 input2  = ILS [("func", [(1,Start,2),(2,Constant "1",3),(3, Junction 4, 6),(4,Constant "2", 5), (5, Junction 3, 2),(6,Finish,0)])] 
 output2 = ISS [("func", [(1, [Start], 2), (2, [Constant "1"], 3), (3, [Junction 4], 6), (4, [Constant "2", Junction 3], 2),(6, [Finish], 0)])] 
 
 input3  = ILS [("main", [(1,Start,2),(2, Call "fun", 3),(3, Output, 4),(4, Finish, 0)]),("fun", [(1,Start,2),(2, Constant "Hello World!", 3),(3, Finish, 0)])] 
 output3 = ISS [("main", [(1,[Start, Call "fun", Output, Finish],0)]),("fun", [(1,[Start, Constant "Hello World!", Finish],0)])]
 
 input4  = ILS [("main",[])]
 output4 = ISS [("main",[])]

 -- testSynAna05 = "SyntactiaclAnalysis: " ~: (erwarteter wert) @=? (SynAna.process eingabe)
 -- ...
 
 testModule = [testSynAna01,testSynAna02,testSynAna03,testSynAna04]
