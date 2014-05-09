module IntermediateCode(process) where

-- imports --
import InterfaceDT as IDT

import LLVM.General.AST
import qualified LLVM.General.AST.Global as Global
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant

-- generate module from list of definitions
generateModule :: [Definition] -> Module
generateModule definitions = defaultModule {
  moduleName = "rail-heaven",
  moduleDefinitions = definitions
}

terminator :: Named Terminator
terminator = Do Ret {
  returnOperand = Nothing,
  metadata' = []
}

generateInstruction (Constant value) =
  Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "push",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }

generateInstruction _ =
  Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "blas",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }


generateBasicBlock :: (Int, [Lexeme], Int) -> BasicBlock
generateBasicBlock (label, instructions, 0) =
  BasicBlock (Name $ "l_" ++ show label) (map generateInstruction instructions) terminator
generateBasicBlock (label, instructions, jumpLabel) =
  BasicBlock (Name $ "l_" ++ show label) (map generateInstruction instructions) branch
  where branch = Do Br {
    dest = Name $ "l_" ++ show jumpLabel,
    metadata' = []
  }

generateBasicBlocks :: [(Int, [Lexeme], Int)] -> [BasicBlock]
generateBasicBlocks = map generateBasicBlock

-- generate function definition from AST
generateFunction :: AST -> Definition
generateFunction (name, lexemes) = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name name,
  Global.returnType = VoidType,
  Global.basicBlocks = generateBasicBlocks lexemes
}

-- generate list of LLVM Definitions from list of ASTs
generateFunctions :: [AST] -> [Definition]
generateFunctions = map generateFunction

-- entry point into module --
process :: IDT.SemAna2InterCode -> IDT.InterCode2CodeOpt
process (IDT.ISI input) = IDT.IIC $ generateModule $ generateFunctions input
