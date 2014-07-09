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
  | duplicatefunctions input = error EH.strDuplicateFunctions
  | nomain input = error EH.strMainMissing
  | not (all validfollowers input) = error EH.strUnknownNode
  | otherwise = IDT.ISI (map fixcalls (concatMap splitlambda $ map check input))

 -- since every function besides main has been renamed i norder to enable lambdas, calls have to be modified
 fixcalls :: IDT.AST -> IDT.AST
 fixcalls (funcname, paths) = (funcname, map fixpathcall paths)
  where
   fixpathcall (id, lexemes, follower) = (id, map fixlistcall lexemes, follower)
   fixlistcall (Call func) = Call (if func == "" then "" else newfunc func)
   fixlistcall lexeme = lexeme
 
 -- we add a g_ to every function name to make sure lambda names do not collide
 newfunc :: String -> String
 newfunc name = if name == "main" then "main" else "g_" ++ name

 -- splits lambdas into own functions
 splitlambda :: IDT.AST -> [IDT.AST]
 splitlambda func@(funcname, paths) = func : lambdafuncs 1
  where
   lambdafuncs offset
    | offset > maximum (fst (getids func)) = []
    | islambda offset = ("l_" ++ funcname ++ "_" ++ show offset, (1, [NOP], offset):tail paths):lambdafuncs (offset + 1)
    | otherwise = lambdafuncs (offset + 1)
   islambda x = any (\(_, lex, _) -> Lambda x `elem` lex) paths

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
 nomain input = "main" `notElem` allfunctions input

 -- checking if there are two or more functions with the same name
 duplicatefunctions :: [IDT.AST] -> Bool
 duplicatefunctions input = length functions > length (nub functions)
  where functions = allfunctions input

 -- getting a list of every function
 allfunctions :: [IDT.AST] -> [String]
 allfunctions [] = []
 allfunctions ((name, _):xs) = name:allfunctions xs
 
 -- this will return the exact same input if it's valid and will error otherwise
 check :: IDT.AST -> IDT.AST
 check (name, nodes) = (name, map checknode nodes)

 -- this will return the exact same input if it's valid and will error otherwise
 checknode :: (Int, [Lexeme], Int) -> (Int, [Lexeme], Int)
 checknode (id, lexeme, following)
   | following == 0 && not (last lexeme `elem` [Finish, Boom] || isinvalidjunction (last lexeme)) = error EH.strInvalidMovement
   | otherwise = (id, map checklexeme lexeme, following)
	where
   isinvalidjunction (Junction x) = x == 0
   isinvalidjunction _ = False

 -- this will return the exact same input if it's valid and will error otherwise
 checklexeme :: Lexeme -> Lexeme
 checklexeme (Junction 0) = error EH.strInvalidMovement
 checklexeme (Push "") = error EH.strInvalidVarName
 checklexeme (Pop "") = error EH.strInvalidVarName
 checklexeme lexeme = lexeme
