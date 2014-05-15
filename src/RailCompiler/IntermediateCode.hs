module IntermediateCode(process) where

-- imports --
import InterfaceDT as IDT

import LLVM.General.AST
import qualified LLVM.General.AST.Global as Global
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant as Constant
import LLVM.General.AST.Linkage
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Operand
import LLVM.General.AST.Instruction as Instruction
import LLVM.General.AST.Float
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

bytePointerType = PointerType {
  pointerReferent = IntegerType 8,
  pointerAddrSpace = AddrSpace 0
}

-- function declaration for puts
puts = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "puts",
  Global.returnType = IntegerType 32,
  Global.parameters = ([ Parameter bytePointerType (UnName 0) [] ], False)
}

push = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "push",
  Global.returnType = VoidType,
  Global.parameters = ([ Parameter bytePointerType (UnName 0) [] ], False)
}

pop = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "pop",
  Global.returnType = bytePointerType,
  Global.parameters = ([], False)
}

peek = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "peek",
  Global.returnType = bytePointerType,
  Global.parameters = ([], False)
}

generateInstruction (Constant value) =
  [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "push",
    arguments = [
          (ConstantOperand Constant.GetElementPtr {
            Constant.inBounds = True,
            Constant.address = Constant.GlobalReference (UnName 0), --TODO look up reference in symbol table
            Constant.indices = [
              Int { integerBits = 8, integerValue = 0 },
              Int { integerBits = 8, integerValue = 0 }
            ]
          }, [])
    ],
    functionAttributes = [],
    metadata = []
  }]

-- depending on the Lexeme we see we need to create one or more Instructions
-- the generateInstruction function should return a list of instructions
-- after the mapping phase we should flatten the array with concat so we that we get
-- a list of Instructions that we can insert in the BasicBlock

-- call puts with top of stack
generateInstruction Output =
  [
  --pop
  UnName 0 := LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "pop",
    arguments = [],
    functionAttributes = [],
    metadata = []
  },
  --call puts with pop result
  Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "puts",
    arguments = [
      (LocalReference $ UnName 0, [])
    ],
    functionAttributes = [],
    metadata = []
  }
  ]

-- do nothing?
--generateInstruction Start =
--  undefined

-- return void?
--generateInstruction Finish = undefined

-- noop
generateInstruction _ = [ Do $ Instruction.FAdd (ConstantOperand $ Float $ Single 1.0) (ConstantOperand $ Float $ Single 1.0) [] ]

isUsefulInstruction Start = False
isUsefulInstruction _ = True

-- removes Lexemes without meaning to us
filterInstrs = filter isUsefulInstruction

generateBasicBlock :: (Int, [Lexeme], Int) -> BasicBlock
generateBasicBlock (label, instructions, 0) =
  BasicBlock (Name $ "l_" ++ show label) (concatMap generateInstruction $ filterInstrs instructions) terminator
generateBasicBlock (label, instructions, jumpLabel) =
  BasicBlock (Name $ "l_" ++ show label) (concatMap generateInstruction $ filterInstrs instructions) branch
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
process (IDT.ISI input) = IDT.IIC $ generateModule $ constants ++ [push, pop, peek, puts] ++ generateFunctions input
  where
    constants = zipWith generateGlobalDefinition [0..] $ generateConstants input
    dict = fromList $ zipWith foo [0..] $ getAllCons input
    foo index (Constant s) = (s, (length s, index))
