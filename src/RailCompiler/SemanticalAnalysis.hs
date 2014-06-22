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
  | null input = error EH.strEmptyProgram
  | nomain input = error EH.strMainMissing
  | not (all validfollowers input) = error EH.strUnknownNode
  | otherwise = IDT.ISI (concatMap splitlambda $ map check input)

 -- splits lambdas into own functions
 splitlambda :: IDT.AST -> [IDT.AST]
 splitlambda func@(funcname, lexemes) = func : lambdafuncs 1
  where
   lambdafuncs offset
    | offset > maximum (fst (getids func)) = []
    | islambda offset = (funcname ++ "!" ++ (show offset), (1, [NOP], offset):tail lexemes):lambdafuncs (offset + 1)
    | otherwise = lambdafuncs (offset + 1)
   islambda x = any (\(_, lex, _) -> Lambda x `elem` lex) lexemes

 -- checks if there are unknown followers
 validfollowers :: IDT.AST -> Bool
 validfollowers ast = null subset
  where
   (ids, followers) = getids ast
   subset = nub followers \\ ids

 -- gets a tuple of (ids, followers) to check if there are unknown followers
 getids :: IDT.AST -> ([Int], [Int])
 getids (_, nodes) = unzip (getnodeids nodes)
  where
   getnodeids [] = []
   getnodeids ((id, nodes, follow):xs) = (0, junctionattribute nodes):(id, follow):getnodeids xs
   junctionattribute nodes = case last nodes of
    (Junction attribute) -> attribute
    _ -> 0

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
 checklexeme (Push "") = error EH.strInvalidVarName
 checklexeme (Pop "") = error EH.strInvalidVarName
 checklexeme lexeme = lexeme
