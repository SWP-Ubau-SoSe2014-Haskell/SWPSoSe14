module ErrorHandling where

-- Common-Errors
generalError = "An error occured! Unfortunately, we can give you no exact indication of the error."

-- PreProcessor-Errors
noStartSymbolFound = "No startsymbol '$' found! You should add a '$' as the startsymbol to your rail program."

-- Lexer errors
strNestedOpenBracket    = "Nested opening bracket in string constant."
strNonSymmetricEscape   = "Non-symmetric escape sequence in string constant."
-- TODO: Would be good to include the unhandled escape sequence.
strUnhandledEscape      = "Unhandled escape sequence in string constant."

-- SyntacticalAnalysis-Errors

-- SemanticalAnalysis-Errors

-- IntermediateCode-Errors

-- CodeOptimization-Errors

-- Backend
