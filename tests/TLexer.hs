module TLexer (
               testModule     -- tests the module Lexer
              )
 where

 -- imports --
 import Test.HUnit
 import InterfaceDT                   as IDT
 import qualified Lexer

 -- functions --
 testLexer01 = "Proper turning: " ~: res [Constant "1"] @=? run [" \\", "  \\   /-t-#", "   ---/--f-#"]
 testLexer02 = "Reflection: " ~: res [Constant "1"] @=? run [" \\", "  \\   #  #  #", "   \\   f f f", "    \\   \\|/", " #t-------@-f#", "         /|\\", "        f f f", "       #  #  #"]
 testLexer03 = "Rail crash: " ~: crash @=? run [" /", "#"]
 testLexer04 = "One liner: " ~: crash @=? run []
 testLexer05 = "Endless loop: " ~: IDT.ILS [("main",[(1,Start,2),(2,Constant "1",3),(3,NOP,3)])] @=? run [" 1 ", "  \\", " @--@"]
 testLexer06 = "Junction test: " ~: IDT.ILS [("main", [(1, Start, 2), (2, Junction 3, 5), (3, Constant "1", 4), (4, Finish, 0), (5, Constant "0", 6), (6, Finish, 0)])] @=? run [" \\", "  \\  /-1#", "   -<", "     \\-0#"]
 testLexer07 = "Simple Junction test: " ~: crash @=? run [" *-1#"]
 testLexer08 = "Two Junctions: " ~: IDT.ILS [("main", [(1, Start, 2), (2, Junction 3, 5), (3, Junction 4, 5), (4, Finish, 0), (5, Constant "0", 6), (6, Finish, 0)])] @=? run [" \\    --\\     -#", "  \\  /   \\   /", "   -<     --<", "     \\       \\", "      ---------0#"]
 testLexer09 = "Merging Junctions: " ~: IDT.ILS [("main", [(1, Start, 2), (2, Junction 3, 3), (3, Finish, 0)])] @=? run [" \\    -\\", "  \\  /  \\", "   -<    -#", "     \\  /", "      -/"]
 testLexer10 = "Push and Pop: " ~: res [Constant "1", Pop "x", Push "x"] @=? run [" \\", "  --1(!x!)(x)#"]
 testLexer11 = "Illegal cross Junctions: " ~: crash @=? run [" \\", "  +-#"]
 testLexer12 = "While: " ~: IDT.ILS [("main", [(1, Start, 2), (2, EOF, 3), (3, Junction 2, 4), (4, Finish, 0)])] @=? run [" \\   /----\\", "  \\  |    |", "   \\ \\    /", "    ---e-<", "          \\-#"]
 testLexer13 = "Empty Junction ends: " ~: IDT.ILS [("main",[(1, Start, 2), (2, Junction 0, 3), (3, Junction 4, 0), (4, Junction 5, 6), (5, Finish, 0), (6, Finish, 0)])] @=? run [" \\", "  \\    /      /--\\   /-#", "   \\--<    --<    --<", "       \\--/   \\      \\-#"]
 testLexer14 = "Turning on Lexeme: " ~: crash @=? run [" \\", "  \\#"]
 testLexer15 = "Lambda: " ~: IDT.ILS [("main",[(1, Start, 2), (2, Lambda 3, 5), (3, Underflow, 4), (4, Finish, 0), (5, Finish, 0)])] @=? run [" \\", "#--&u#"]
 testLexer16 = "Empty Lambda: " ~: IDT.ILS [("main",[(1, Start, 2), (2, Lambda 0, 3), (3, Finish, 0)])] @=? run [" \\", "#--&"]

 -- helper functions
 run :: IDT.Grid2D -> IDT.Lexer2SynAna
 run grid = Lexer.process (IDT.IPL [("$ 'main'":grid, 0)])

 res :: [Lexeme] -> IDT.Lexer2SynAna
 res lexeme = IDT.ILS [("main", (1, Start, 2):nodes 2 lexeme)]
  where
   nodes i [] = [(i, Finish, 0)]
   nodes i (x:xs) = (i, x, i+1):nodes (i+1) xs

 crash :: IDT.Lexer2SynAna
 crash = IDT.ILS [("main", [(1, Start, 0)])]
 
 testModule = [testLexer01, testLexer02, testLexer03, testLexer04, testLexer05, testLexer06, testLexer07, testLexer08, testLexer09, testLexer10, testLexer11, testLexer12, testLexer13, testLexer14, testLexer15, testLexer16]
