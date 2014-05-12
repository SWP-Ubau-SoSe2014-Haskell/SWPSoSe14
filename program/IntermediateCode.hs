module IntermediateCode(process) where

-- imports --
import InterfaceDT as IDT

import LLVM.General.AST
import qualified LLVM.General.AST.Global as Global
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant
import LLVM.General.AST.Linkage
import Data.Char
import Data.Map hiding (filter, map)

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

createGlobalString :: Lexeme -> Global
createGlobalString (Constant s) = globalVariableDefaults {
  Global.type' = ArrayType {nArrayElements = fromInteger l, elementType = IntegerType {typeBits = 8}},
  Global.initializer = Just Array {
    memberType = IntegerType {typeBits = 8},
    memberValues = map trans s
  }}
  where
    l = toInteger $ length s
    trans c = Int {integerBits = 8, integerValue = toInteger $ ord c}

generateConstants :: [AST] -> [Global]
generateConstants = map createGlobalString . getAllCons

getAllCons :: [AST] -> [Lexeme]
getAllCons = concatMap generateCons

generateCons :: AST -> [Lexeme]
generateCons (name, paths) = concatMap generateC paths

generateC :: (Int, [Lexeme], Int) -> [Lexeme]
generateC (pathID, lex, nextPath) = filter checkCons lex
checkCons (Constant c) = True
checkCons _ = False



-- function declaration for putchar
putchar = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "putchar",
  Global.returnType = IntegerType 32,
  Global.parameters = ([ Parameter (IntegerType 32) (UnName 0) [] ], False)
}

generateInstruction (Constant value) =
  Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "putchar",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }

-- depending on the Lexeme we see we need to create one or more Instructions
-- the generateInstruction function should return a list of instructions
-- after the mapping phase we should flatten the array with concat so we that we get
-- a list of Instructions that we can insert in the BasicBlock

-- call putchar with top of stack
generateInstruction Output =
  undefined

-- do nothing?
--generateInstruction Start =
--  undefined

-- return void?
generateInstruction Finish = undefined

generateInstruction _ =
  Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "putchar",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }

isUsefulInstruction Start = False
isUsefulInstruction _ = True

-- removes Lexemes without meaning to us
filterInstrs = filter isUsefulInstruction

generateBasicBlock :: (Int, [Lexeme], Int) -> BasicBlock
generateBasicBlock (label, instructions, 0) =
  BasicBlock (Name $ "l_" ++ show label) (map generateInstruction $ filterInstrs instructions) terminator
generateBasicBlock (label, instructions, jumpLabel) =
  BasicBlock (Name $ "l_" ++ show label) (map generateInstruction $ filterInstrs instructions) branch
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

generateGlobalDefinition :: Integer -> Global -> Definition
generateGlobalDefinition index def = GlobalDefinition def {
  Global.name = UnName $ fromInteger index,
  Global.isConstant = True,
  Global.linkage = Internal,
  Global.hasUnnamedAddr = True
}

-- entry point into module --
process :: IDT.SemAna2InterCode -> IDT.InterCode2CodeOpt
process (IDT.ISI input) = IDT.IIC $ generateModule $ constants ++ [putchar] ++ generateFunctions input
  where
    constants = zipWith generateGlobalDefinition [0..] $ generateConstants input
    dict = fromList $ zipWith foo [0..] $ getAllCons input
    foo index (Constant s) = (s, (length s, index))
