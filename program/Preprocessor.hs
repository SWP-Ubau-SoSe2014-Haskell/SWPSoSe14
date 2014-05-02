{- |
Module      :  Preprocessor.hs
Description :  .
Copyright   :  (c) Christopher Pockrandt, Nicolas Lehmann
License     :  MIT

Stability   :  stable

Preprocessor gets the content of the input file and puts each rail function
into a list of strings such that the first character of the first line is a
dollar sign. Leading lines without a dollar sign in the input file are removed.
-}

module Preprocessor (
                     process   -- main function of the module "Preprocessor"
                    )
 where
 
 -- imports --
 import InterfaceDT as IDT
 
 -- functions --
 process :: IDT.Input2PreProc -> IDT.PreProc2Lexer
 process (IDT.IIP input) = (IDT.IPL output)
  where
   output = (groupFunctionsToGrid2Ds . removeLines. lines) input

 -- |Removes all leading strings from list until first string begins with a
 -- dollar sign.
 removeLines :: Grid2D -> Grid2D
 removeLines [] = []
 removeLines (line:lines)
  | null line = removeLines lines
  | head line == '$' = line:lines
  | otherwise = removeLines lines
		   
 -- |Puts every rail function/program into its on grid such that the dollar
 -- sign is the first character in the first line.
 groupFunctionsToGrid2Ds :: Grid2D -> [Grid2D]
 groupFunctionsToGrid2Ds grid = helperGFtG2D grid [] []
  where
   helperGFtG2D :: Grid2D -> Grid2D -> [Grid2D] -> [Grid2D]
   helperGFtG2D [] [] akk2 = akk2
   helperGFtG2D [] akk1 akk2 = akk2 ++ [akk1]
   helperGFtG2D ([]:rest) akk1 akk2 = helperGFtG2D rest (akk1++[[]]) akk2
   helperGFtG2D (('$':zeilenrest):rest) [] akk2 = helperGFtG2D rest ['$':zeilenrest] akk2
   helperGFtG2D (('$':zeilenrest):rest) akk1 akk2 = helperGFtG2D rest ['$':zeilenrest] (akk2 ++ [akk1])
   helperGFtG2D (zeile:rest) akk1 akk2 = helperGFtG2D rest (akk1++[zeile]) akk2