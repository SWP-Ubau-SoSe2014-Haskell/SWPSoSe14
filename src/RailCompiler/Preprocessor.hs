{- |
Module      :  Preprocessor.hs
Description :  .
Maintainer  :  (c) Christopher Pockrandt, Nicolas Lehmann
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
 import ErrorHandling as EH
 import Data.List
 
 -- functions --
 process :: IDT.Input2PreProc -> IDT.PreProc2Lexer
 process (IDT.IIP input) = IDT.IPL output
  where
   output = map maximize $ (groupFunctionsToGrid2Ds . removeLines . lines) input

 -- |Makes the first line as long as max(max(lines),#lines)
 -- this is useful for the lexer to determine an upper bound for empty endless loops
 maximize :: (Grid2D, Int) -> (Grid2D, Int)
 maximize ([], offset) = ([], offset)
 maximize (x:xs, offset) = (stretchto (max maxlines maxcols) x:xs, offset)
  where
   stretchto count line = take count (line ++ repeat ' ')
   maxlines = maximum $ map length (x:xs)
   maxcols = length (x:xs)

 -- |Return False iff the first character is a dollar sign.
 notStartingWithDollar :: String -> Bool
 notStartingWithDollar x = null x || head x /= '$'
 
 -- |Removes all leading strings from list until first string begins with a
 -- dollar sign.
 removeLines :: Grid2D -> (Grid2D, Int)
 removeLines grid
  | null $ fst $ result grid 0 = error noStartSymbolFound
  | otherwise = result grid 0
   where
    result grid n
     | null grid = (grid, n)
     | not $ notStartingWithDollar $ head grid = (grid, n)
     | otherwise = result (tail grid) (n + 1)

 -- |Puts every rail function/program into its on grid such that the dollar
 -- sign is the first character in the first line.
 groupFunctionsToGrid2Ds :: (Grid2D, Int) -> [(Grid2D, Int)]
 groupFunctionsToGrid2Ds ([], _) = []
 groupFunctionsToGrid2Ds (grid, offset) = (head grid:func, offset):groupFunctionsToGrid2Ds (other, offset + 1 + length func)
  where
   (func, other) = span notStartingWithDollar $ tail grid
