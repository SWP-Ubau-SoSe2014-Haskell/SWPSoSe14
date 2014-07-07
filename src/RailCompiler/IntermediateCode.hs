{- |
Module      :  IntermediateCode.hs
Description :  Intermediate code generation
Maintainer  :  Philipp Borgers, Tilman Blumenbach, Lyudmila Vaseva, Sascha Zinke,
               Maximilian Claus, Michal Ajchman, Nicolas Lehmann, Tudor Soroceanu
License     :  MIT
Stability   :  unstable

IntemediateCode.hs takes the output from the SemanticalAnalysis module
(which is a list of paths) and generates LLVM IR code.
It turns every path of the form (PathID, [Lexeme], PathID) into a basic block.

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IntermediateCode(process) where

-- imports --
import ErrorHandling as EH
import InterfaceDT as IDT
import FunctionDeclarations
import TypeDefinitions

import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.List
import Data.Map hiding (filter, map)
import Data.Word
import LLVM.General.AST
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant as Constant
import LLVM.General.AST.Float
import LLVM.General.AST.Instruction as Instruction
import LLVM.General.AST.IntegerPredicate
import LLVM.General.AST.Linkage
import LLVM.General.AST.Operand
import qualified LLVM.General.AST.Global as Global

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

-- |Generate module from list of definitions.
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

-- |Generate global byte array (from a constant string).
createGlobalString :: Lexeme -> Global
createGlobalString (Constant s) = globalVariableDefaults {
  Global.type' = ArrayType {
    nArrayElements = fromInteger l,
    elementType = IntegerType {typeBits = 8}
  },
  Global.initializer = Just Array {
    memberType = IntegerType {typeBits = 8},
    memberValues = map trans s ++ [Int { integerBits = 8, integerValue = 0 }]
  }
}
  where
    l = toInteger $ 1 + length s
    trans c = Int { integerBits = 8, integerValue = toInteger $ ord c }

-- |Create constant strings/byte arrays for a module.
-- TODO: Maybe rename these subfunctions?
generateConstants :: [AST] -> [Global]
generateConstants = map createGlobalString . getAllCons

-- |Get all 'Constant's in a module.
getAllCons :: [AST] -> [Lexeme]
getAllCons = concatMap generateCons
  where
    generateCons :: AST -> [Lexeme]
    generateCons (name, paths) = concatMap generateC paths

    generateC :: (Int, [Lexeme], Int) -> [Lexeme]
    generateC (pathID, lex, nextPath) = filter checkCons lex

    checkCons :: Lexeme -> Bool
    checkCons (Constant c) = True
    checkCons _ = False

--------------------------------------------------------------------------------
-- |Generate global variables for push and pop from and into variables.
createGlobalVariable :: Lexeme -> Global
createGlobalVariable (Pop v) = globalVariableDefaults {
  Global.name = Name v,
  Global.type' = ArrayType {
    nArrayElements = fromInteger l,
    elementType = IntegerType {typeBits = 8}
  },
  Global.initializer = Just Array {
    memberType = IntegerType {typeBits = 8},
    memberValues = map trans v ++ [Int { integerBits = 8, integerValue = 0 }]
  }
}
  where
    l = toInteger $ 1 + length v
    trans c = Int { integerBits = 8, integerValue = toInteger $ ord c }

-- |Generate all global variable definitions for a module.
generateVariables :: [AST] -> [Global]
generateVariables = map createGlobalVariable . getAllVars
  where
    getAllVars :: [AST] -> [Lexeme]
    getAllVars = concatMap generateVars

    generateVars :: AST -> [Lexeme]
    generateVars (name, paths) = nub $ concatMap generateV paths --delete duplicates

    generateV :: (Int, [Lexeme], Int) -> [Lexeme]
    generateV (pathID, lex, nextPath) = filter checkVars lex

    checkVars :: Lexeme -> Bool
    checkVars (Pop v) = True
    checkVars _ = False

--------------------------------------------------------------------------------

-- |Struct declaration for the symbol table
structTable :: Definition
structTable = TypeDefinition (Name "struct.table")
      (Just $ StructureType False
                [ PointerType (IntegerType 8) (AddrSpace 0), 
                  PointerType (IntegerType 8) (AddrSpace 0), 
                  PointerType (NamedTypeReference $ Name "struct.table") (AddrSpace 0)])

-- |Generate an instruction for the 'u'nderflow check command.
generateInstruction :: Lexeme -> Codegen [Named Instruction]
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

-- |Generate instructions for junctions.
generateInstruction (Junction label) = do
  index <- fresh
  index2 <- fresh
  return [ UnName index := LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "pop_bool",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }, UnName index2 := LLVM.General.AST.ICmp {
    LLVM.General.AST.iPredicate = LLVM.General.AST.IntegerPredicate.EQ,
    LLVM.General.AST.operand0 = LocalReference (UnName index),
    LLVM.General.AST.operand1 = ConstantOperand $ Int 64 0,
    metadata = []
  }]

-- |Generate instruction for pop into a variable
generateInstruction (Pop name) = do
  index <- fresh
  return [ UnName index := LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "pop_into",
    arguments = [ (LocalReference $ Name "table", []),
    (ConstantOperand Constant.GetElementPtr {
      Constant.inBounds = True,
      Constant.address = Constant.GlobalReference (Name name),
      Constant.indices = [
       Int { integerBits = 8, integerValue = 0 },
       Int { integerBits = 8, integerValue = 0 }
      ]}, [])],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for push from a variable
generateInstruction (Push name) = do
  index <- fresh
  return [ UnName index := LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "push_from",
    arguments = [(LocalReference $ Name "table", []),
      (ConstantOperand Constant.GetElementPtr {
      Constant.inBounds = True,
      Constant.address = Constant.GlobalReference (Name name),
      Constant.indices = [
       Int { integerBits = 8, integerValue = 0 },
       Int { integerBits = 8, integerValue = 0 }
      ]}, [])],
    functionAttributes = [],
    metadata = []
  }]


-- |Generate instruction for push of a constant.
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
    function = Right $ ConstantOperand $ GlobalReference $ Name "push_string_cpy",
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
    arguments = [(ConstantOperand $ Int 1 1, [])],
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

-- |Generate instruction for the EOF lexeme.
generateInstruction EOF =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "eof_check",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for the add instruction.
generateInstruction Add1 =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "add",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for the remainder instruction.
generateInstruction Remainder =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "rem",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for the sub instruction.
generateInstruction Subtract =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "sub",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for the mul instruction.
generateInstruction Multiply =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "mult",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for the div instruction.
generateInstruction Divide =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "div",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for the strlen instruction.
generateInstruction Size =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "strlen",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for the strapp instruction.
generateInstruction Append =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "strapp",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for the strcut instruction.
generateInstruction Cut =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "strcut",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for the equal instruction.
generateInstruction Equal =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "equal",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for the greater instruction.
generateInstruction Greater =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "greater",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for start instruction
generateInstruction Start = do
  index <- fresh
  index2 <- fresh
  return [ UnName index := LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "start",
    arguments = [],
    functionAttributes = [],
    metadata = []
  },
    Name "table" := Instruction.Alloca {
    allocatedType = NamedTypeReference $ Name "struct.table",
    numElements = Nothing,
    alignment = 4,
    metadata = []
  },
    UnName index2 := LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "initialise",
    arguments = [(LocalReference $ Name "table", [])],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for finish instruction
generateInstruction Finish =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name "finish",
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Generate instruction for function call
generateInstruction (IDT.Call functionName) =
  return [Do LLVM.General.AST.Call {
    isTailCall = False,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ GlobalReference $ Name functionName,
    arguments = [],
    functionAttributes = [],
    metadata = []
  }]

-- |Fallback for unhandled lexemes (generates no-op).
generateInstruction _ = return [ Do $ Instruction.FAdd (ConstantOperand $ Float $ Single 1.0) (ConstantOperand $ Float $ Single 1.0) [] ]

-- |Generate the instructions making up one basic block.
generateBasicBlock :: (Int, [Lexeme], Int) -> Codegen BasicBlock
generateBasicBlock (label, instructions, 0) = do
  tmp <- mapM generateInstruction instructions
  return $ BasicBlock (Name $ "l_" ++ show label) (concat tmp) $ terminator 0
generateBasicBlock (label, instructions, jumpLabel) = do
  tmp <- mapM generateInstruction instructions
  i <- gets count
  case filter isJunction instructions of
    [Junction junctionLabel] -> return $ BasicBlock (Name $ "l_" ++ show label) (concat tmp) $ condbranch junctionLabel i
    [] -> return $ BasicBlock (Name $ "l_" ++ show label) (concat tmp) branch
  where
    isJunction (Junction a) = True
    isJunction _ = False
    condbranch junctionLabel i = Do CondBr {
      condition = LocalReference $ UnName i,
      trueDest = Name $ "l_" ++ show junctionLabel,
      falseDest = Name $ "l_" ++ show jumpLabel,
      metadata' = []
    }
    branch = Do Br {
      dest = Name $ "l_" ++ show jumpLabel,
      metadata' = []
    }

-- |Generate all basic blocks for a function.
generateBasicBlocks :: [(Int, [Lexeme], Int)] -> Codegen [BasicBlock]
generateBasicBlocks = mapM generateBasicBlock

-- |Generate a function definition from an AST.
generateFunction :: AST -> GlobalCodegen Definition
generateFunction (name, lexemes) = do
  dict <- gets dict
  return $ GlobalDefinition $ Global.functionDefaults {
    Global.name = Name name,
    Global.returnType = IntegerType 32,
    Global.basicBlocks = execCodegen dict $ generateBasicBlocks lexemes
  }

-- |Create a new local variable (?).
fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

-- |Generate list of LLVM Definitions from list of ASTs
generateFunctions :: [AST] -> GlobalCodegen [Definition]
generateFunctions = mapM generateFunction

-- |Generate a global definition for a constant.
--
-- This is an unnamed, global constant, i. e. it has a numeric name
-- like '@0'.
generateGlobalDefinition :: Integer -> Global -> Definition
generateGlobalDefinition index def = GlobalDefinition def {
  Global.name = UnName $ fromInteger index,
  Global.isConstant = True,
  Global.linkage = Internal,
  Global.hasUnnamedAddr = True
}

-- |Generate a global definition for a variable name.
--
-- Such definitions are used to pass variable names to
-- LLVM backend functions like 'pop_into()'.
generateGlobalDefinitionVar :: Integer -> Global -> Definition
generateGlobalDefinitionVar i def = GlobalDefinition def {
  Global.isConstant = True,
  Global.linkage = Internal,
  Global.hasUnnamedAddr = True
}

-- |Entry point into module.
process :: IDT.SemAna2InterCode -> IDT.InterCode2CodeOpt
process (IDT.ISI input) = IDT.IIC $ generateModule $ constants ++ variables ++ 
    [ stackElementTypeDef, structTable, underflowCheck, FunctionDeclarations.print, crash, start, finish, inputFunc,
      eofCheck, pushStringCpy, pop, peek, add, sub, rem1, mul, div1, streq, strlen, strapp, strcut,
      popInt, equal, greater, popInto, pushFrom, popBool, initialiseSymbolTable ] ++ codegen input
  where
    constants = zipWith generateGlobalDefinition [0..] $ generateConstants input
    variables = zipWith generateGlobalDefinitionVar [0..] $ generateVariables input
    constantPool = fromList $ zipWith createConstantPoolEntry [0..] $ getAllCons input
    createConstantPoolEntry index (Constant s) = (s, (length s, index))
    codegen input = execGlobalCodegen constantPool $ generateFunctions input

-- vim:ts=2 sw=2 et
