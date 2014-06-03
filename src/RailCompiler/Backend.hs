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
process :: IDT.CodeOpt2Backend -> IDT.Backend2Output
process input = output
  where
    output = IDT.IBO $ generateOutput input

generateOutput :: IDT.CodeOpt2Backend -> IO String
generateOutput (IDT.ICB mod) = do
  s <- withContext $ \context ->
    runErrorT $ withModuleFromAST context mod $ \m -> moduleLLVMAssembly m
  case s of
    Left err -> return err
    Right ll -> return ll
