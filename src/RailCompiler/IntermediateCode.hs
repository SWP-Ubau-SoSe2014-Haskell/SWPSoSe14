{- |
Module      :  IntermediateCode.hs
Description :  Intermediate code generation
Copyright   :  (c) AUTHORS
License     :  MIT
Stability   :  unstable
-}

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

-- generate a ret void
terminator :: Named Terminator
terminator = Do Ret {
  returnOperand = Nothing,
  metadata' = []
}

-- generate global byte array (constant string)
createGlobalString :: Lexeme -> Global
createGlobalString (Constant s) = globalVariableDefaults {
  Global.type' = ArrayType {
    nArrayElements = fromInteger l,
    elementType = IntegerType {typeBits = 8}
  },
  Global.initializer = Just Array {
    memberType = IntegerType {typeBits = 8},
    memberValues = map trans s
  }
}
  where
    l = toInteger $ length s
    trans c = Int { integerBits = 8, integerValue = toInteger $ ord c }

-- create constant strings/byte arrays for module
-- TODO maybe rename these subfunctions?
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

-- pointer type for i8* used e.g. as "string" pointer
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

-- function declaration for push
push = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "push",
  Global.returnType = VoidType,
  Global.parameters = ([ Parameter bytePointerType (UnName 0) [] ], False)
}

-- function declaration for pop
pop = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "pop",
  Global.returnType = bytePointerType,
  Global.parameters = ([], False)
}

-- function declaration for peek
peek = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "peek",
  Global.returnType = bytePointerType,
  Global.parameters = ([], False)
}

-- generate instruction for push of a constant
-- access to our push function definied in stack.ll
-- http://llvm.org/docs/LangRef.html#call-instruction
generateInstruction (Constant value) =
  [Do LLVM.General.AST.Call {
    -- The optional tail and musttail markers indicate that the optimizers
    --should perform tail call optimization.
    isTailCall = False,
    -- The optional "cconv" marker indicates which calling convention the call
    -- should use. If none is specified, the call defaults to using C calling
    -- conventions.
    callingConvention = C,
    -- The optional Parameter Attributes list for return values. Only 'zeroext',
    -- 'signext', and 'inreg' attributes are valid here
    returnAttributes = [],
    -- actual function to call
    function = Right $ ConstantOperand $ GlobalReference $ Name "push",
    -- argument list whose types match the function signature argument types
    -- and parameter attributes. All arguments must be of first class type. If
    -- the function signature indicates the function accepts a variable number of
    -- arguments, the extra arguments can be specified.
    arguments = [
          -- The 'getelementptr' instruction is used to get the address of a
          -- subelement of an aggregate data structure. It performs address 
          -- calculation only and does not access memory.
          -- http://llvm.org/docs/LangRef.html#getelementptr-instruction
          (ConstantOperand Constant.GetElementPtr {
            Constant.inBounds = True,
            Constant.address = Constant.GlobalReference (UnName 0), --TODO look up reference in symbol table
            Constant.indices = [
              Int { integerBits = 8, integerValue = 0 },
              Int { integerBits = 8, integerValue = 0 }
            ]
          }, [])
    ],
    -- optional function attributes list. Only 'noreturn', 'nounwind',
    -- 'readonly' and 'readnone' attributes are valid here.
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
  UnName 0 := LLVM.General.AST.Call { --FIXME UnName 0 is a hack we need to keep track of local refs
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
      (LocalReference $ UnName 0, []) --FIXME UnName 0 is a hack we need to keep track of local refs
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
    foo index (Constant s) = (s, (length s, index)) --TODO rename foo to something meaningful e.g. createSymTable
