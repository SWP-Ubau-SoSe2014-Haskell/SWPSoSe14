module TPreProc (
                    testModule     -- tests the module Preprocessor
                   )
 where

 -- imports --
 import Test.HUnit
 import InterfaceDT                   as IDT
 import qualified Preprocessor        as PreProc
 
 -- functions --
 testPreProc01   = "PreProc: " ~: (IDT.IPL []) @=? (PreProc.process $ IDT.IIP "")
 testPreProc02   = "PreProc: " ~: (IDT.IPL []) @=? (PreProc.process $ IDT.IIP "a\nb\n")
 testPreProc03   = "PreProc: " ~: (IDT.IPL [["$1"], ["$2"]]) @=? (PreProc.process $ IDT.IIP "$1\n$2\n")
 testPreProc04   = "PreProc: " ~: (IDT.IPL [["$1"], ["$2", "", "", ""], ["$3", "", "", "", ""]]) @=? (PreProc.process $ IDT.IIP "$1\n$2\n\n\n\n$3\n\n\n\n\n")
 testPreProc05   = "PreProc: " ~: (IDT.IPL [["$2"]]) @=? (PreProc.process $ IDT.IIP " $1\n$2\n")
  
 testModule = [testPreProc01,testPreProc02,testPreProc03,testPreProc04,testPreProc05]
                        
