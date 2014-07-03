{- |
Module      :  TpyeDefinitions.hs
Description :  type definitions for intermediate code generation
Maintainer  :  Philipp Borgers, Tilman Blumenbach, Lyudmila Vaseva, Sascha Zinke,
               Maximilian Claus, Michal Ajchman, Nicolas Lehmann, Tudor Soroceanu
License     :  MIT
Stability   :  unstable

All type definitions live here.

-}

module TypeDefinitions where

import LLVM.General.AST
import LLVM.General.AST.AddrSpace

-- |Opaque type definition for the stack_element struct, defined in stack.ll.
stackElementTypeDef :: Definition
stackElementTypeDef = TypeDefinition (Name "stack_element") Nothing

-- |Pointer type for 'i8*' used e.g. as "string" pointer
bytePointerType :: Type
bytePointerType = PointerType {
  pointerReferent = IntegerType 8,
  pointerAddrSpace = AddrSpace 0
}

-- |Pointer type for 'i8**' used as variable pointer
bytePointerTypeVar :: Type
bytePointerTypeVar = PointerType {
  pointerReferent = PointerType {
    pointerReferent = IntegerType 8,
    pointerAddrSpace = AddrSpace 0
  },
  pointerAddrSpace = AddrSpace 0
}

-- |Pointer type: %stack_element* (see stack.ll).
stackElementPointerType :: Type
stackElementPointerType = PointerType {
    pointerReferent = NamedTypeReference $ Name "stack_element",
    pointerAddrSpace = AddrSpace 0
}
