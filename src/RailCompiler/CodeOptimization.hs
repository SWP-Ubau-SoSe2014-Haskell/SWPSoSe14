{- |
Module      : SemanticalAnalysis.hs
Description : .
Maintainer  : Christopher Pockrandt
License     : MIT

This module might be used for optimizing intermediate llvm code one day (or not).
Until then, `process` equals the identity function.

-}module CodeOptimization (
                         process   -- main function of the module "CodeOptimization"
					    )
 where
 
 -- imports --
 import InterfaceDT as IDT
 import ErrorHandling as EH
 
 -- functions --
 process :: IDT.InterCode2CodeOpt -> IDT.CodeOpt2Backend
 process (IDT.IIC input) = IDT.ICB output
  where
   output = input
