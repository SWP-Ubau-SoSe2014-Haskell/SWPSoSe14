module TestPreProc (
                    testModule     -- tests the module Preprocessor
                   )
 where

 -- imports --
 import Test.HUnit
 import InterfaceDT                   as IDT
 import qualified Preprocessor        as PreProc
 
 -- functions --
 testPreProc01 = "PreProcessor: " ~: (IDT.IPL [["$"]]) @=? (PreProc.process (IDT.IIP "fdh$"))
 -- testPreProc02 = "PreProcessor: " ~: (erwarteter wert) @=? (PreProc.process eingabe)
 -- testPreProc03 = "PreProcessor: " ~: (erwarteter wert) @=? (PreProc.process eingabe)
 -- testPreProc04 = "PreProcessor: " ~: (erwarteter wert) @=? (PreProc.process eingabe)
 -- testPreProc05 = "PreProcessor: " ~: (erwarteter wert) @=? (PreProc.process eingabe)
 -- ...
  
 testModule = [testPreProc01] --,testPreProc02,testPreProc03,testPreProc04,testPreProc05]
                        