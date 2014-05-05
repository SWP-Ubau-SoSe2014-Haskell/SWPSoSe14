module IntermediateCode(process) where

-- imports --
import InterfaceDT as IDT

import LLVM.General.AST
import qualified LLVM.General.AST as AST

initModule :: AST.Module
initModule = defaultModule { moduleName = "rail-heaven" }

-- functions --
process :: IDT.SemAna2InterCode -> AST.Module
process input = initModule
