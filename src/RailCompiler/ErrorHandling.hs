{- |
Module      : ErrorHandling.hs
Description : .
Maintainer  : (c) Christopher Pockrandt, Nicolas Lehmann
License     : MIT

Contains error messages for errors during the compilation of a rail program
that result in an immediate stop of the process. The purpose is to get an
overview of all possible error messages for negative test cases for integration
testing.

-}
module ErrorHandling where

-- Common-Errors
generalError = "An error occured! Unfortunately, we can give you no exact indication of the error."

-- PreProcessor-Errors
noStartSymbolFound = "No startsymbol '$' found! You should add a '$' as the startsymbol to your rail program."

-- Lexer errors
strFunctionNameMissing	= "Function without name found."
strNestedOpenBracket    = "Nested opening bracket in string constant."
strNonSymmetricEscape   = "Non-symmetric escape sequence in string constant."
strUnhandledEscape      = "Unhandled escape sequence `\\%c' in string constant."
strMissingClosingBracket= "Closing Bracket not found."
strInvalidVarName       = "Invalid Variable Name used."
strInvalidFuncName      = "Invalid Function Name used."

-- "shr" like in "shared graph representation".
shrLineNoLexeme         = "No lexeme found in line: %s"

-- SyntacticalAnalysis-Errors

-- SemanticalAnalysis-Errors
strInvalidMovement      = "Invalid movement."
strMainMissing          = "No 'main' Method found."
strUnknownNode          = "Unknown Node in AST."
strEmptyProgram         = "Empty Program."
strDuplicateFunctions   = "Duplicate Functions found."

-- IntermediateCode-Errors

-- CodeOptimization-Errors

-- Backend
