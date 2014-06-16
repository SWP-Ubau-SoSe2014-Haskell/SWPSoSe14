{- |
Module      : SemanticalAnalysis.hs
Description : .
Maintainer  : Christopher Pockrandt
License     : MIT

There is no need for a semantical analysis at this time, therefore the function
`process` equals the identity function

-}
module SemanticalAnalysis (
                           process   -- main function of the module "SemanticalAnalysis"
                          )
 where
 
 -- imports --
 import InterfaceDT as IDT
 import ErrorHandling as EH
 
 -- functions --
 process :: IDT.SynAna2SemAna -> IDT.SemAna2InterCode
 process (IDT.ISS input) = IDT.ISI (map check input)
 
 -- this will return the exact same input if it's valid and will error otherwise
 check :: IDT.AST -> IDT.AST
 check (name, nodes) = (name, map checknode nodes)

 -- this will return the exact same input if it's valid and will error otherwise
 checknode :: (Int, [Lexeme], Int) -> (Int, [Lexeme], Int)
 checknode (id, lexeme, following)
   | following == 0 && not (last lexeme == Finish || isvalidjunction (last lexeme)) = error EH.strInvalidMovement
   | otherwise = (id, map checklexeme lexeme, following)
	where
   isvalidjunction (Junction x) = x /= 0
   isvalidjunction _ = False

 -- this will return the exact same input if it's valid and will error otherwise
 checklexeme :: Lexeme -> Lexeme
 checklexeme (Junction 0) = error EH.strInvalidMovement
 checklexeme lexeme = lexeme
