module TestPreProc (
                    testModule     -- tests the module Preprocessor
                   )
 where

 -- imports --
 import Test.HUnit
 import InterfaceDT                   as IDT
 import qualified Preprocessor        as PreProc
 
 -- functions --
 testPreProc01   = "PreProc: " ~: ([]) @=? (process "")
 testPreProc02   = "PreProc: " ~: ([]) @=? (process "a\nb\n")
 testPreProc03   = "PreProc: " ~: ([["$1"], ["$2"]]) @=? (process "$1\n$2\n")
 testPreProc04   = "PreProc: " ~: ([["$1"], ["$2", "", "", ""], ["$3", "", "", "", ""]]) @=? (process "$1\n$2\n\n\n\n$3\n\n\n\n\n")
 testPreProc05   = "PreProc: " ~: ([["$2"]]) @=? (process " $1\n$2\n")
  
 testModule = [testPreProc01,testPreProc02,testPreProc03,testPreProc04,testPreProc05]
                        
