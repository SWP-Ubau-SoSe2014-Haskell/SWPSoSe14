{- |
Module      :  IntermediateCode.hs
Description :  Intermediate code generation
Copyright   :  (c) AUTHORS
License     :  MIT
Stability   :  unstable
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IntermediateCode(process) where

-- imports --
import InterfaceDT as IDT
import ErrorHandling as EH

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
import Data.Word
import Data.Map hiding (filter, map)

import Control.Monad.State
import Control.Applicative

data CodegenState = CodegenState {
  blocks :: [BasicBlock],
  count :: Word, --Count of unnamed Instructions
  localDict :: Map String (Int, Integer)
}

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

data GlobalCodegenState = GlobalCodegenState {
  dict :: Map String (Int, Integer)
}

newtype GlobalCodegen a = GlobalCodegen { runGlobalCodegen :: State GlobalCodegenState a }
  deriving (Functor, Applicative, Monad, MonadState GlobalCodegenState)

execGlobalCodegen :: Map String (Int, Integer) -> GlobalCodegen a -> a
execGlobalCodegen d m = evalState (runGlobalCodegen m) $ GlobalCodegenState d

execCodegen :: Map String (Int, Integer) -> Codegen a -> a
execCodegen d m = evalState (runCodegen m) $ CodegenState [] 0 d

-- generate module from list of definitions
generateModule :: [Definition] -> Module
generateModule definitions = defaultModule {
  moduleName = "rail-heaven",
  moduleDefinitions = definitions
}

-- |Generate a ret statement, returning a 32-bit Integer to the caller.
-- While we use 64-bit integers everywhere else, our "main" function
-- needs to return an "int" which usually is 32-bits even on 64-bit systems.
terminator :: Integer -- ^The 32-bit Integer to return.
    -> Named Terminator -- ^The return statement.
terminator ret = Do Ret {
  returnOperand = Just $ ConstantOperand $ Int 32 ret,
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

-- |Function declaration for 'underflow_check'.
underflowCheck = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "underflow_check",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'print'.
print = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "print",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'crash'.
crash = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "crash",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'input'.
inputFunc = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "input",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
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

-- |Generate an instruction for the 'u'nderflow check command.
generateInstruction Underflow =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "underflow_check",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- generate instruction for push of a constant
-- access to our push function definied in stack.ll??
-- http://llvm.org/docs/LangRef.html#call-instruction
generateInstruction (Constant value) = do
  index <- fresh
  dict <- gets localDict
  return [UnName index := LLVM.General.AST.Call {
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
            Constant.address = Constant.GlobalReference (UnName $ fromInteger $ snd $ dict ! value),
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

-- |Generate instruction for printing strings to stdout.
generateInstruction Output =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "print",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for the Boom lexeme (crashes program).
generateInstruction Boom =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "crash",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for the Input lexeme.
generateInstruction Input =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "input",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- do nothing?
--generateInstruction Start =
--  undefined

-- return void?
--generateInstruction Finish = undefined

-- noop
generateInstruction _ = return [ Do $ Instruction.FAdd (ConstantOperand $ Float $ Single 1.0) (ConstantOperand $ Float $ Single 1.0) [] ]

isUsefulInstruction Start = False
isUsefulInstruction _ = True

-- removes Lexemes without meaning to us
filterInstrs = filter isUsefulInstruction

generateBasicBlock :: (Int, [Lexeme], Int) -> Codegen BasicBlock
generateBasicBlock (label, instructions, 0) = do
  tmp <- mapM generateInstruction $ filterInstrs instructions
  return $ BasicBlock (Name $ "l_" ++ show label) (concat tmp) $ terminator 0
generateBasicBlock (label, instructions, jumpLabel) = do
  tmp <- mapM generateInstruction $ filterInstrs instructions
  return $ BasicBlock (Name $ "l_" ++ show label) (concat tmp) branch
  where branch = Do Br {
    dest = Name $ "l_" ++ show jumpLabel,
    metadata' = []
  }


generateBasicBlocks :: [(Int, [Lexeme], Int)] -> Codegen [BasicBlock]
generateBasicBlocks = mapM generateBasicBlock

-- generate function definition from AST
generateFunction :: AST -> GlobalCodegen Definition
generateFunction (name, lexemes) = do
  dict <- gets dict
  return $ GlobalDefinition $ Global.functionDefaults {
    Global.name = Name name,
    Global.returnType = IntegerType 32,
    Global.basicBlocks = execCodegen dict $ generateBasicBlocks lexemes
  }

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

-- generate list of LLVM Definitions from list of ASTs
generateFunctions :: [AST] -> GlobalCodegen [Definition]
generateFunctions = mapM generateFunction

generateGlobalDefinition :: Integer -> Global -> Definition
generateGlobalDefinition index def = GlobalDefinition def {
  Global.name = UnName $ fromInteger index,
  Global.isConstant = True,
  Global.linkage = Internal,
  Global.hasUnnamedAddr = True
}

-- entry point into module --
process :: IDT.SemAna2InterCode -> IDT.InterCode2CodeOpt
process (IDT.ISI input) = IDT.IIC $ generateModule $ constants ++
    [underflowCheck, IntermediateCode.print, crash, inputFunc, push, pop, peek] ++
    generateFunctionsFoo input
  where
    constants = zipWith generateGlobalDefinition [0..] $ generateConstants input
    d = fromList $ zipWith foo [0..] $ getAllCons input
    foo index (Constant s) = (s, (length s, index)) --TODO rename foo to something meaningful e.g. createSymTable
    generateFunctionsFoo input = execGlobalCodegen d $ generateFunctions input
