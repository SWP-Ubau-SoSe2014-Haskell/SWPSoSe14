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

-- "shr" like in "shared graph representation".
shrLineNoLexeme         = "No lexeme found in line: %s"

-- SyntacticalAnalysis-Errors

-- SemanticalAnalysis-Errors

-- IntermediateCode-Errors

-- CodeOptimization-Errors

-- Backend
