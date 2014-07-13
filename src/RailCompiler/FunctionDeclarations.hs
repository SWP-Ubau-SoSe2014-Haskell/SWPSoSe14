{- |
Module      :  FunctionDeclarations.hs
Description :  Function declarations for intermediate code generation
Maintainer  :  Philipp Borgers, Tilman Blumenbach, Lyudmila Vaseva, Sascha Zinke,
               Maximilian Claus, Michal Ajchman, Nicolas Lehmann, Tudor Soroceanu
License     :  MIT
Stability   :  unstable

All function declarations live here.

-}

module FunctionDeclarations where

import LLVM.General.AST.AddrSpace
import LLVM.General.AST
import qualified LLVM.General.AST.Global as Global

import TypeDefinitions

-- |Function declaration for 'start'.
start :: Definition
start = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "start",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'underflow_check'.
underflowCheck :: Definition
underflowCheck = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "underflow_check",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'print'.
print :: Definition
print = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "print",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'crash'.
crash :: Definition
crash = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "crash",
  Global.returnType = VoidType,
  Global.parameters = ([ Parameter (IntegerType 1) (Name "is_custom_error") [] ], False)
}

-- |Function declaration for 'finish'.
finish :: Definition
finish = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "finish",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'input'.
inputFunc :: Definition
inputFunc = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "input",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'eof_check'.
eofCheck :: Definition
eofCheck = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "eof_check",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'add'.
add :: Definition
add = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "add",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'rem'.
rem1 :: Definition
rem1 = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "rem",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'sub'.
sub :: Definition
sub = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "sub",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'mul'.
mul :: Definition
mul = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "mult",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'div'.
div1 :: Definition
div1 = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "div",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'type'.
type1 :: Definition
type1 = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "type",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for pushing constants.
pushStringCpy :: Definition
pushStringCpy = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "push_string_cpy",
  Global.returnType = stackElementPointerType,
  Global.parameters = ([ Parameter bytePointerType (UnName 0) [] ], False)
}

-- |Function declaration for 'pop'.
pop :: Definition
pop = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "pop",
  Global.returnType = bytePointerType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'peek'
peek :: Definition
peek = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "peek",
  Global.returnType = bytePointerType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'streq'
streq :: Definition
streq = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "streq",
  Global.returnType = bytePointerType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'strlen'
strlen :: Definition
strlen = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "strlen",
  Global.returnType = bytePointerType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'strapp'
strapp :: Definition
strapp = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "strapp",
  Global.returnType = bytePointerType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'strcut'.
strcut :: Definition
strcut = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "strcut",
  Global.returnType = bytePointerType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'pop_int'
popInt :: Definition
popInt = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "pop_int",
  Global.returnType = IntegerType 64,
  Global.parameters = ([], False)
}

-- |Function declaration for 'pop_bool'
popBool :: Definition
popBool = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "pop_bool",
  Global.returnType = IntegerType 64,
  Global.parameters = ([], False)
}

-- |Function declaration for 'equal'
equal :: Definition
equal = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "equal",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'greater'
greater :: Definition
greater = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "greater",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for 'pop_into'
popInto :: Definition
popInto = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "pop_into",
  Global.returnType = VoidType,
  Global.parameters = ( [ Parameter (PointerType (NamedTypeReference $ Name  
    "struct.table") (AddrSpace 0)) (UnName 0) [], 
    Parameter bytePointerType (UnName 0) [] ], False)
}

-- |Function declaration for 'push_from'
pushFrom :: Definition
pushFrom = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "push_from",
  Global.returnType = VoidType,
  Global.parameters = ( [ Parameter (PointerType (NamedTypeReference $ Name  
    "struct.table") (AddrSpace 0)) (UnName 0) [], 
    Parameter bytePointerType (UnName 0) [] ], False)
}

-- |Function declaration for initialising of the symbol table
initialiseSymbolTable :: Definition
initialiseSymbolTable = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "initialise",
  Global.returnType = VoidType,
  Global.parameters = ([ Parameter (PointerType (NamedTypeReference $ 
    Name "struct.table") (AddrSpace 0)) (UnName 0) [] ], False)
}

-- |Function declaration for malloc
malloc :: Definition
malloc = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "malloc",
  Global.returnType = bytePointerType,
  Global.parameters = ([ Parameter (IntegerType 64) (UnName 0) [] ], False)
}

-- |Function declaration for copying of the symbol table
copySymbolTable :: Definition
copySymbolTable = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "copy_symbol_table",
  Global.returnType = VoidType,
  Global.parameters = ([ Parameter (PointerType (NamedTypeReference $ 
    Name "struct.table") (AddrSpace 0)) (UnName 0) [],
    Parameter (PointerType (NamedTypeReference $ 
    Name "struct.table") (AddrSpace 0)) (UnName 0) [] ], False)
}

-- |Function declaration for pushing nil onto the stack.
listPushNil :: Definition
listPushNil = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "gen_list_push_nil",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for list cons.
listCons :: Definition
listCons = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "gen_list_cons",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}

-- |Function declaration for list breakup.
listBreakup :: Definition
listBreakup = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name "gen_list_breakup",
  Global.returnType = VoidType,
  Global.parameters = ([], False)
}
