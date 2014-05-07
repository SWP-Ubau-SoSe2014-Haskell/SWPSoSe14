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
 
 import Data.List
 
 -- functions --
 process :: IDT.Input2PreProc -> IDT.PreProc2Lexer
 process (IDT.IIP input) = (IDT.IPL output)
  where
   output = (groupFunctionsToGrid2Ds . removeLines. lines) input

 -- |Logical xor
 xor :: Bool -> Bool -> Bool
 xor = (\x y -> not (x == y))

 notStartingWithDollar :: String -> Bool
 notStartingWithDollar = (\x -> null x || head x /= '$')
 -- |Removes all leading strings from list until first string begins with a
 -- dollar sign.
 removeLines :: Grid2D -> Grid2D
 removeLines = dropWhile notStartingWithDollar

 -- |Puts every rail function/program into its on grid such that the dollar
 -- sign is the first character in the first line.
 groupFunctionsToGrid2Ds :: Grid2D -> [Grid2D]
 groupFunctionsToGrid2Ds = groupBy (\x y -> notStartingWithDollar x `xor` notStartingWithDollar y)
