{- |
Module      : Backend
Description : Converts the internal LLVM representation into textual LLVM IR.
Maintainer  : Tilman Blumenbach, Nicolas Lehmann, Philipp Borgers
License     : MIT

Uses the LLVM bindings for Haskell to convert the internal LLVM representation
(provided by the bindings themselves) into the final, textual LLVM IR.

Does not do any linking.
-}
module Backend (
                process   -- main function of the module "Backend"
               )
where

-- imports --
import ErrorHandling as EH
import InterfaceDT as IDT

import Control.Monad.Error
import LLVM.General.AST
import LLVM.General.AST.Global
import LLVM.General.Context
import LLVM.General.Module
import qualified LLVM.General.AST as AST

-- functions --
-- |Converts the internal LLVM representation into textual LLVM IR.
process :: IDT.InterCode2Backend -> IDT.Backend2Output
process input = IDT.IBO $ generateOutput input

-- |Uses the Haskell LLVM bindings to convert the internal LLVM
-- representation into textual LLVM IR.
generateOutput :: IDT.InterCode2Backend -> IO String
generateOutput (IDT.IIB mod) = do
  s <- withContext $ \context ->
    runErrorT $ withModuleFromAST context mod $ \m -> moduleLLVMAssembly m
  either error return s
