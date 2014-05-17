module TestLexer (
                  testModule     -- tests the module Lexer
                 )
 where

 -- imports --
 import Test.HUnit
 import InterfaceDT                   as IDT
 import qualified Lexer

 -- functions --
 testLexer01 = "Proper turning: " ~: (res [Constant "1"]) @=? (run [" \\", "  \\   /-t-#", "   ---/--f-#"])
 testLexer02 = "Reflection: " ~: (res [Constant "1"]) @=?
 (run [" \\", "  \\   #  #  #", "   \\   f f f", "    \\   \\|/", " #t-------@-f#", "         /|\\", "        f f f", "       #  #  #"])
 testLexer03 = "Rail crash: " ~: (IDT.ILS ("main", [(1, Start, 0)])) @=? (run [" /"])
 -- testLexer04 = "Lexer: " ~: (erwarteter wert) @=? (Lexer.process eingabe)
 -- testLexer05 = "Lexer: " ~: (erwarteter wert) @=? (Lexer.process eingabe)
 -- ...

 -- helper functions
 run :: IDT.Graph -> IDT.Lexer2SynAna
 run graph = Lexer.process (IDT.IPL ["$ 'main'":graph])

 res :: [Lexeme] -> IDT.Lexer2SynAna
 res [lexeme] = IDT.ILS ("main", (1, Start, 2):(nodes 2 lexeme))
  where
   nodes i [] = [(i, Finish, 0)]
   nodes i (x:xs) = (i, x, i+1):(nodes (i+1) xs)
 
 testModule = [testLexer01, testLexer02, testLexer03]
