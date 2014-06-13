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
 
 -- functions

 -- identity function
 process :: IDT.SynAna2SemAna -> IDT.SemAna2InterCode
 process (IDT.ISS input) = IDT.ISI output
  where
   output = input
