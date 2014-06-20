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
 import Data.List
 
 -- functions --
 process :: IDT.SynAna2SemAna -> IDT.SemAna2InterCode
 process (IDT.ISS input)
  | nomain input = error EH.strMainMissing
  | not (validfollowers input) = error EH.strUnknownNode
  | otherwise = IDT.ISI (map check input)

 -- checks if there are unknown followers
 validfollowers :: [IDT.AST] -> Bool
 validfollowers ast = null subset
  where
   (ids, followers) = getids ast
   subset = (nub followers) \\ ids

 -- gets a tuple of (ids, followers) to check if there are unknown followers
 getids :: [IDT.AST] -> ([Int], [Int])
 getids [] = ([0], [])
 getids ((_, nodes):xs) = tuplemerge (unzip (getnodeids nodes)) (getids xs)
  where
   getnodeids [] = []
   getnodeids ((id, _, follow):xs) = (id, follow):getnodeids xs
   tuplemerge (lhs1, rhs1) (lhs2, rhs2) = (lhs1 ++ lhs2, rhs1 ++ rhs2)

 -- looking for a main function
 nomain :: [IDT.AST] -> Bool
 nomain [] = True
 nomain ((name, _):xs)
  | name == "main" = False
	| otherwise = nomain xs
 
 -- this will return the exact same input if it's valid and will error otherwise
 check :: IDT.AST -> IDT.AST
 check (name, nodes) = (name, map checknode nodes)

 -- this will return the exact same input if it's valid and will error otherwise
 checknode :: (Int, [Lexeme], Int) -> (Int, [Lexeme], Int)
 checknode (id, lexeme, following)
   | following == 0 && not (last lexeme `elem` [Finish, Boom] || isvalidjunction (last lexeme)) = error EH.strInvalidMovement
   | otherwise = (id, map checklexeme lexeme, following)
	where
   isvalidjunction (Junction x) = x /= 0
   isvalidjunction _ = False

 -- this will return the exact same input if it's valid and will error otherwise
 checklexeme :: Lexeme -> Lexeme
 checklexeme (Junction 0) = error EH.strInvalidMovement
 checklexeme lexeme = lexeme
