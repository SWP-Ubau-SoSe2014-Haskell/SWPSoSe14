module IntermediateCode(process) where

-- imports --
import InterfaceDT as IDT

import LLVM.General.AST
import qualified LLVM.General.AST as AST
import LLVM.General.AST.Global

-- generate module from list of definitions
generateModule :: [Definition] -> AST.Module
generateModule definitions = defaultModule {
  moduleName = "rail-heaven",
  moduleDefinitions = definitions
}

-- generate function definition from AST
generateFunction :: AST -> Definition
generateFunction (name, lexemes) = GlobalDefinition $ functionDefaults {
  name = Name name,
  returnType = VoidType
}

-- generate list of LLVM Definitions from list of ASTs
generateFunctions :: [AST] -> [Definition]
generateFunctions = map generateFunction

-- entry point into module --
process :: IDT.SemAna2InterCode -> IDT.InterCode2CodeOpt
process (IDT.ISI input) = IDT.IIC $ generateModule $ generateFunctions input
