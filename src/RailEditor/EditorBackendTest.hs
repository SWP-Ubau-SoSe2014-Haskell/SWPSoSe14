module Main where

import Test.HUnit
import EditorBackend

--begin: Test-Field operations
testCoord = (0,100)
testChar = Undefined '$'
testBool = True
testField = (Field testChar testBool)

testGetFieldChar = TestCase (assertEqual "Test: getFieldChar" '$' (getFieldChar testField))

testGetFieldVisited = TestCase (assertEqual "Test: getFieldVisited" testBool (getFieldVisited testField))

fieldTestSuite = [TestLabel "Test: getFieldChar" testGetFieldChar, TestLabel "Test: getFieldVisited" testGetFieldVisited]
--end: Test-Field operations
--begin: Test-Function operations
testName = "main"
testFields = [[(Field (Undefined '$') True)],[(Field (Undefined '\\') False)]]
testFunction = Function testName testFields

testGetFunctionFields = TestCase (assertEqual "Test: getFunctionFields" testFields (getFunctionFields testFunction))

testGetFunctionName = TestCase (assertEqual "Test: getFunctionName" testName (getFunctionName testFunction))

functionTestSuite = [TestLabel "Test: getFunctionFields" testGetFunctionFields, TestLabel "Test: getFunctionName" testGetFunctionName]
--end: Test-Function operations
--begin: Test-Programm operations
testProgramm = Programm [testFunction]

testGetProgrammFunctions = TestCase (assertEqual "Test: getProgrammFunctions" [testFunction] (getProgrammFunctions testProgramm))

programmTestSuite = [TestLabel "Test: getProgrammFunctions" testGetProgrammFunctions]
--end: Test-Programm operations
--begin: Test-Move operations
testMoveAllowed = Allowed (1,1)
testMoveForbidden = Forbidden (0,-1)

testIsAllowed = TestCase ( do a <- return $ isAllowed testMoveAllowed
                              assertEqual "Test: isAllowed with 'Allowed' input" True a
                              f <- return $ isAllowed testMoveForbidden
                              assertEqual "Test: isAllowed with 'Forbidden' input" False f 
                         )

testFromAllowed = TestCase (assertEqual "Test: fromAllowed" (1,1) (fromAllowed testMoveAllowed))

moveTestSuite = [TestLabel "Test: isAllowed" testIsAllowed, TestLabel "Test: fromAllowed" testFromAllowed]
--end: Test-Move operations
--begin: Test Operations for converting code Strings to the ADT 'Programm'
testString = "$ 'main'"
expectedFields = [Field (Undefined '$') False,Field (Undefined ' ') False,Field (Undefined '\'') False,Field (Undefined 'm') False,Field (Undefined 'a') False,Field (Undefined 'i') False,Field (Undefined 'n') False,Field (Undefined '\'') False]

testCode = ["$ 'main'"," \\","  \\--[Test]-o#"]
expectedListOfFields = [[Field (Undefined '$') False,Field (Undefined ' ') False,Field (Undefined '\'') False,Field (Undefined 'm') False,Field (Undefined 'a') False,Field (Undefined 'i') False,Field (Undefined 'n') False,Field (Undefined '\'') False],[Field (Undefined ' ') False,Field (Undefined '\\') False],[Field (Undefined ' ') False,Field (Undefined ' ') False,Field (Undefined '\\') False,Field (Undefined '-') False,Field (Undefined '-') False,Field (Undefined '[') False,Field (Undefined 'T') False,Field (Undefined 'e') False,Field (Undefined 's') False,Field (Undefined 't') False,Field (Undefined ']') False,Field (Undefined '-') False,Field (Undefined 'o') False,Field (Undefined '#') False]]

testListWithEmptyElements = [["","main",""],[],["usw...",""]]
expectedCleanedList = [["main"],["usw..."]]

testCode2 = ["$ 'main'"," \\","  \\--[Test]-o#","$ 'f2'"]
testExpectedSplit = [["$ 'main'"," \\","  \\--[Test]-o#"],["$ 'f2'"]]

testExpectedProgramm = Programm [Function "main" [[Field (Undefined '$') False,Field (Undefined ' ') False,Field (Undefined '\'') False,Field (Undefined 'm') False,Field (Undefined 'a') False,Field (Undefined 'i') False,Field (Undefined 'n') False,Field (Undefined '\'') False],[Field (Undefined ' ') False,Field (Undefined '\\') False],[Field (Undefined ' ') False,Field (Undefined ' ') False,Field (Undefined '\\') False,Field (Undefined '-') False,Field (Undefined '-') False,Field (Undefined '[') False,Field (Undefined 'T') False,Field (Undefined 'e') False,Field (Undefined 's') False,Field (Undefined 't') False,Field (Undefined ']') False,Field (Undefined '-') False,Field (Undefined 'o') False,Field (Undefined '#') False]],Function "f2" [[Field (Undefined '$') False,Field (Undefined ' ') False,Field (Undefined '\'') False,Field (Undefined 'f') False,Field (Undefined '2') False,Field (Undefined '\'') False]]]

testCodeToProgramm = TestCase (assertEqual "Test: codeToProgramm" testExpectedProgramm (codeToProgramm testCode2))

testSplitContentInChunks = TestCase (assertEqual "Test: splitContentInChunks" testExpectedSplit (splitContentInChunks testCode2))

testFilterEmptyElements = TestCase (assertEqual "Test: filterEmptyElements" expectedCleanedList (filterEmptyElements testListWithEmptyElements))

testReadFunctionCodeToAdtFunction = TestCase (assertEqual "Test: readFunctionCodeToAdtFunction" expectedListOfFields (readFunctionCodeToAdtFunction testCode 0))

testReadStringToFieldList = TestCase (assertEqual "Test: readStringToFieldList" expectedFields (readStringToFieldList testString (0,0)))

codeToProgrammTestSuite = [TestLabel "Test: codeToProgramm" testCodeToProgramm,TestLabel "Test: splitContentInChunks" testSplitContentInChunks,TestLabel "Test: filterEmptyElements" testFilterEmptyElements,TestLabel "Test: readFunctionCodeToAdtFunction" testReadFunctionCodeToAdtFunction, TestLabel "Test: readStringToFieldList" testReadStringToFieldList]
--end: Test Operations for converting code Strings to the ADT 'Programm'
--begin: Test Operations for modify and access 'Field' in a structure
expectedMarkedFunction =Function testName [[(Field (Undefined '$') True)],[(Field (Undefined '\\') True)]]

testGetFieldByCoord = TestCase (assertEqual "Test: getFieldByCoord" (Field (Undefined '\\') False) (getFieldByCoord testFields (0,1)))

testIsVisited = TestCase (assertEqual "Test: isVisited" False (isVisited (0,1) testFunction))

testMarkAsVisited = TestCase (assertEqual "Test: markAsVisited" expectedMarkedFunction (markAsVisited (0,1) testFunction))

modifyAccessFieldTestSuite = [TestLabel "Test: getFieldByCoord" testGetFieldByCoord,TestLabel "Test: isVisited" testIsVisited,TestLabel "Test: markAsVisited" testMarkAsVisited]
--end: Test Operations for modify and access 'Field' in a structure
--begin: Test Functions to decide how the train will move
testGetWestMove = TestCase (do a <- return $ getWestMove (1,0) [[(Field (Undefined '\\') False),(Field (Undefined '-') False)]] True
                               assertEqual "Test: getWestMove Allowed" (Allowed (0,0)) a
                               b <- return $ getWestMove (0,0) [[(Field (Undefined '-') False)]] True
                               assertEqual "Test: getWestMove Forbidden" (Forbidden (0,0)) b
                           )

testGetNorthWestMove = TestCase (do a <- return $ getNorthWestMove (1,2) [[],[(Field (Undefined '\\') False)],[(Field (Undefined ' ') False),(Field (Undefined '\\') False)]] True
                                    assertEqual "Test: getNorthWestMove primary route" (Allowed (0,1)) a
                                    b <- return $ getNorthWestMove (1,1) [[],[(Field (Undefined '-') False),(Field (Undefined '\\') False)]] True
                                    assertEqual "Test: getNorthWestMove west secondary route" (Allowed (0,1)) b
                                    c <- return $ getNorthWestMove (0,2) [[],[(Field (Undefined '|') False)],[(Field (Undefined '\\') False)]] True
                                    assertEqual "Test: getNorthWestMove north secondary route" (Allowed (0,1)) c 
                                    d <- return $ getNorthWestMove (0,2) [[],[(Field (Undefined ' ') False),(Field (Undefined '|') False)],[(Field (Undefined '-') False),(Field (Undefined '\\') False)]] True
                                    assertEqual "Test: getNorthWestMove secondary conflict" (Forbidden (0,2)) d
                                )

testGetNorthEastMove = TestCase (do a <- return $ getNorthEastMove (0,2) [[],[(Field (Undefined ' ') False),(Field (Undefined '/') False)],[(Field (Undefined '/') False)]] True
                                    assertEqual "Test: getNorthEastMove primary route" (Allowed (1,1)) a
                                    b <- return $ getNorthEastMove (0,1) [[],[(Field (Undefined '/') False),(Field (Undefined '-') False)]] True
                                    assertEqual "Test: getNorthEastMove east secondary route" (Allowed (1,1)) b
                                    c <- return $ getNorthEastMove (0,2) [[],[(Field (Undefined '|') False)],[(Field (Undefined '/') False)]] True
                                    assertEqual "Test: getNorthEastMove north secondary route" (Allowed (0,1)) c 
                                    d <- return $ getNorthEastMove (0,2) [[],[(Field (Undefined '|') False)],[(Field (Undefined '/') False),(Field (Undefined '-') False)]] True
                                    assertEqual "Test: getNorthEastMove secondary conflict" (Forbidden (0,2)) d
                                )

testGetSouthWestMove = TestCase (do a <- return $ getSouthWestMove (1,0) [[(Field (Undefined ' ') False),(Field (Undefined '/') False)],[(Field (Undefined '/') False)]] True
                                    assertEqual "Test: getSouthWestMove primary route" (Allowed (0,1)) a
                                    b <- return $ getSouthWestMove (1,0) [[(Field (Undefined '-') False),(Field (Undefined '/') False)]] True
                                    assertEqual "Test: getSouthWestMove west secondary route" (Allowed (0,0)) b
                                    c <- return $ getSouthWestMove (0,0) [[(Field (Undefined '/') False)],[(Field (Undefined '|') False)]] True
                                    assertEqual "Test: getSouthWestMove south secondary route" (Allowed (0,1)) c 
                                    d <- return $ getSouthWestMove (1,0) [[(Field (Undefined '-') False),(Field (Undefined '/') False)],[(Field (Undefined ' ') False),(Field (Undefined '|') False)]] True
                                    assertEqual "Test: getSouthWestMove secondary conflict" (Forbidden (1,0)) d
                                )

testGetSouthEastMove = TestCase (do a <- return $ getSouthEastMove (0,0) [[(Field (Undefined '\\') False),(Field (Undefined '-') False)],[(Field (Undefined ' ') False),(Field (Undefined '\\') False)]] True
                                    assertEqual "Test: getSouthEastMove primary route" (Allowed (1,1)) a
                                    b <- return $ getSouthEastMove (0,0) [[(Field (Undefined '\\') False),(Field (Undefined '-') False)]] True
                                    assertEqual "Test: getSouthEastMove east secondary route" (Allowed (1,0)) b
                                    c <- return $ getSouthEastMove (0,0) [[(Field (Undefined '\\') False)],[(Field (Undefined '|') False)]] True
                                    assertEqual "Test: getSouthEastMove south secondary route" (Allowed (0,1)) c 
                                    d <- return $ getSouthEastMove (0,0) [[(Field (Undefined '\\') False),(Field (Undefined '-') False)],[(Field (Undefined '|') False)]] True
                                    assertEqual "Test: getSouthEastMove secondary conflict" (Forbidden (0,0)) d
                                )

moveTestSuit = [TestLabel "Test: getWestMove" testGetWestMove, TestLabel "Test: getNorthWestMove" testGetNorthWestMove, TestLabel "Test: getNorthEastMove" testGetNorthEastMove, TestLabel "Test: getSouthWestMove" testGetSouthWestMove,TestLabel "Test: getSouthEastMove" testGetSouthEastMove]
--end: Test Functions to decide how the train will move

superTestSuite = TestList (fieldTestSuite ++ functionTestSuite ++ programmTestSuite ++ moveTestSuite ++ codeToProgrammTestSuite ++ modifyAccessFieldTestSuite ++ moveTestSuit)

main = do runTestTT superTestSuite
