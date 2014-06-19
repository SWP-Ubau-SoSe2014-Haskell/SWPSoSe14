{- |
Module      : Backend
Description : Converts the internal LLVM representation into textual LLVM IR.
Maintainer  : See the AUTHORS file in the root directory of this project for a list
              of contributors.
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
import InterfaceDT as IDT
import ErrorHandling as EH

import LLVM.General.AST
import qualified LLVM.General.AST as AST
import LLVM.General.Context
import LLVM.General.Module
import Control.Monad.Error
import LLVM.General.AST.Global


-- functions --
-- |Converts the internal LLVM representation into textual LLVM IR.
process :: IDT.CodeOpt2Backend -> IDT.Backend2Output
process input = output
  where
    output = IDT.IBO $ generateOutput input

-- |Uses the Haskell LLVM bindings to convert the internal LLVM
-- representation into textual LLVM IR.
generateOutput :: IDT.CodeOpt2Backend -> IO String
generateOutput (IDT.ICB mod) = do
  s <- withContext $ \context ->
    runErrorT $ withModuleFromAST context mod $ \m -> moduleLLVMAssembly m
  case s of
    Left err -> return err
    Right ll -> return ll
