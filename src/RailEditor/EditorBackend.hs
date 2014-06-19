{-# LANGUAGE DeriveDataTypeable#-}
module EditorBackend where

import Data.Typeable (Typeable)
import Data.List.Split (split,keepDelimsL,whenElt,splitWhen)
import Data.Data (Data,toConstr)
import Data.Maybe

-- !!!please dont create dependencies to this module without an announcement, because it is still in the creating and refactoring process!!!

-- Represents a charakter of a programm with the charakter (Char), his coordinate inside the function relative to the $ charakter and a Bool (for determine in the parsing process, whether the process already visited the field)
data Field = Field HighlightChar Bool deriving (Show,Eq)
-- Represents a function, with the name (String) and the Code ([[Field]])
data Function = Function String [[Field]] deriving (Show,Eq)
-- Represents a Programm, which is a set of Functions
data Programm = Programm [Function] deriving (Show,Eq)
-- Represents the next move of the 'train'
data Move = Allowed (Int,Int) | Forbidden (Int,Int) deriving (Show,Eq)
-- Represents the direction of the 'train'
data Direction = North | South | West | East | NorthWest | NorthEast | SouthWest | SouthEast deriving (Show,Eq)
-- Define Highlighting Classes for making highlight with an xml-config file possible in a later version (e.g. <MathOp color=#FF0011> ...)
data HighlightChar =  Undefined Char | MathOp Char | StringOp Char | Misc Char | Constant Char | FunctionUse Char | SystemOp Char | VariableOp Char | ListOp Char | Conditional Char | Rail Char | Cross Char | If Char | Mixed Char deriving (Show, Eq, Data, Typeable)

--begin: HighlightChar operations
fromHighlightChar :: HighlightChar -> Char
fromHighlightChar (Undefined c) = c
fromHighlightChar (MathOp c) = c
fromHighlightChar (StringOp c) = c
fromHighlightChar (Misc c) = c
fromHighlightChar (Constant c) = c
fromHighlightChar (FunctionUse c) = c
fromHighlightChar (SystemOp c) = c
fromHighlightChar (VariableOp c) = c
fromHighlightChar (ListOp c) = c
fromHighlightChar (Conditional c) = c
fromHighlightChar (Rail c) = c
fromHighlightChar (Cross c) = c
fromHighlightChar (If c) = c
fromHighlightChar (Mixed c) = c
--end: HighlightChar operations

--begin: Field operations
getFieldChar :: Field -> Char
getFieldChar (Field  hChar _) = fromHighlightChar hChar

getFieldVisited :: Field -> Bool
getFieldVisited (Field _ visited) = visited
--end: Field operations

--begin: Function operations
getFunctionFields :: Function -> [[Field]]
getFunctionFields (Function _ fields) = fields

getFunctionName :: Function -> String
getFunctionName (Function name _) = name
--end: Function operations

--begin: Programm operations
getProgrammFunctions :: Programm -> [Function]
getProgrammFunctions (Programm functions) = functions
--end: Programm operations

--begin: Move operations
isAllowed :: Move -> Bool
isAllowed (Forbidden _) = False
isAllowed (Allowed _) = True

fromAllowed :: Move -> (Int,Int)
fromAllowed (Allowed coord) = coord
--end: Move operations

--begin: Operations for converting code - Strings to the ADT 'Programm'
codeToProgramm :: [String] -> Programm
codeToProgramm content = Programm $ map getFunctionByContentChunk $ splitContentInChunks content

splitContentInChunks :: [String] -> [[String]]
splitContentInChunks content = filterEmptyElements $ (split . keepDelimsL . whenElt) isStartSymbol content
    where isStartSymbol line | null line = False
                             | otherwise = (\(x:xs) -> x == '$') line

filterEmptyElements :: [[String]] -> [[String]]
filterEmptyElements = map (filter (/="")).filter (/=[])

getFunctionByContentChunk :: [String] -> Function
getFunctionByContentChunk functionCode@(x:xs) = Function functioName $ readFunctionCodeToAdtFunction functionCode 0
    where functioName = splitWhen (=='\'') x !! 1

readFunctionCodeToAdtFunction :: [String] -> Int -> [[Field]]
readFunctionCodeToAdtFunction [] _ = []
readFunctionCodeToAdtFunction (e:es) lineCount = readStringToFieldList e (0,lineCount) :  readFunctionCodeToAdtFunction es (succ lineCount)

readStringToFieldList :: String -> (Int,Int) -> [Field]
readStringToFieldList [] _ = []
readStringToFieldList (e:es) (x,y) = Field (Undefined e) False : readStringToFieldList es (succ x,y)
--end: Operations for converting code - Strings to the ADT 'Programm'
--begin: Operations for modify and access 'Field' in a structure
getFieldByCoord :: [[Field]] -> (Int,Int) -> Field
getFieldByCoord fields (x,y) = (fields !! y) !! x

isVisited :: (Int,Int) -> Function -> Bool
isVisited (x,y) (Function _ fields) = b 
           where (Field _ b) = getFieldByCoord fields (x,y)


markAsVisited :: (Int,Int) -> Function -> Function
markAsVisited (x,y) (Function n fields) = Function n $ up ++ ((left ++ [Field e True] ++ tail right) : tail down)
           where (up,down) = splitAt y fields
                 (left,right) = splitAt x (head down)
                 (Field e _) = head right

setFieldAt :: Move -> Function -> Field -> Function
setFieldAt (Allowed (x,y)) (Function n fields) field = Function n $ up ++ ((left ++ [field] ++ tail right) : tail down)
           where (up,down) = splitAt y fields
                 (left,right) = splitAt x (head down)

--end: Operations for modify and access 'Field' in a structure
--begin: Functions to decide how the train will move
getNextMove :: (Int,Int) -> Direction -> Function -> Bool -> Move
getNextMove (x,y) direction (Function name fields) includeSecondaryBranch = case direction of
                North      -> getNorthMove (x,y) fields includeSecondaryBranch
                South      -> getSouthMove (x,y) fields includeSecondaryBranch
                West       -> getWestMove (x,y) fields includeSecondaryBranch
                East       -> getEastMove (x,y) fields includeSecondaryBranch
                NorthWest  -> getNorthWestMove (x,y) fields includeSecondaryBranch
                NorthEast  -> getNorthEastMove (x,y) fields includeSecondaryBranch
                SouthWest  -> getSouthWestMove (x,y) fields includeSecondaryBranch
                SouthEast  -> getSouthEastMove (x,y) fields includeSecondaryBranch

getNorthMove :: (Int,Int) -> [[Field]] -> Bool -> Move
getNorthMove (x,0) _ includeSecondaryBranch= Forbidden (x,0)
getNorthMove (x,1) _ includeSecondaryBranch= Forbidden (x,1)
getNorthMove (x,y) fields includeSecondaryBranch= Allowed (x,pred y)

getSouthMove :: (Int,Int) -> [[Field]] -> Bool -> Move
getSouthMove (x,y) fields includeSecondaryBranch
        | length fields <= y' = Forbidden (x,y)
        | otherwise = Allowed (x,y')
            where y' = succ y

getEastMove :: (Int,Int) -> [[Field]] -> Bool -> Move
getEastMove (x,y) fields includeSecondaryBranch
        | length (fields !! y) <= x' = Forbidden (x,y)
        | otherwise = Allowed (succ x,y)
            where x' = succ x

getWestMove :: (Int,Int) -> [[Field]] -> Bool -> Move
getWestMove (0,y) _ includeSecondaryBranch = Forbidden (0,y)
getWestMove (x,y) _ includeSecondaryBranch = Allowed (pred x,y)

{-
        Primary:    Secondary:      Secondary2:
          \            --\              |
           \                            \
-}
getNorthWestMove :: (Int,Int) -> [[Field]] -> Bool -> Move
getNorthWestMove (x,y) fields includeSecondaryBranch
        | primary /= ' ' = Allowed (x',y')
        | includeSecondaryBranch && (secondary `xor` secondary2) = Allowed (getSecondary secondary (x',y) secondary2 (x,y'))
        | otherwise = Forbidden (x,y)
            where x' = pred x
                  y' = pred y
                  primary | x <= 0 || y <= 1 || length (fields !! y') <= x' = ' '
                          | otherwise =getFieldChar $ getFieldByCoord fields (x',y')
                  secondary = secondaryChar == '-'
                  secondary2 = secondaryChar2 == '|'
                  secondaryChar | x == 0 = ' '
                            	| otherwise = getFieldChar $ getFieldByCoord fields (x',y)
                  secondaryChar2 | y <= 1 || length (fields !! y') <= x = ' '
                             	 | otherwise = getFieldChar $ getFieldByCoord fields (x,y')

{-
        Primary:     Secondary:      Secondary2:
           /             /-              |
          /                              /
-}

getNorthEastMove :: (Int,Int) -> [[Field]] -> Bool -> Move
getNorthEastMove (x,y) fields includeSecondaryBranch
        | primary /= ' ' = Allowed (x',y')
        | includeSecondaryBranch && (secondary `xor` secondary2) = Allowed (getSecondary secondary (x',y) secondary2 (x,y'))
        | otherwise = Forbidden (x,y)
            where x' = succ x
                  y' = pred y
                  primary | y <= 1 || length (fields !! y') <= x' = ' '
                          | otherwise = getFieldChar $ getFieldByCoord fields (x',y')
                  secondary = secondaryChar == '-'
                  secondary2 = secondaryChar2 == '|'
                  secondaryChar | length (fields !! y) <= x' = ' '
                            | otherwise = getFieldChar $ getFieldByCoord fields (x',y)
                  secondaryChar2 | y <= 1 || length (fields !! y') <= x = ' '
                             | otherwise = getFieldChar $ getFieldByCoord fields (x,y')
{-
     Primary:       Secondary:      Secondary2:
       /               -/               /
      /                                 |
-}

getSouthWestMove :: (Int,Int) -> [[Field]] -> Bool -> Move
getSouthWestMove (x,y) fields includeSecondaryBranch
        | primary /= ' ' = Allowed (x',y')
        | includeSecondaryBranch && (secondary `xor` secondary2) = Allowed (getSecondary secondary (x',y) secondary2 (x,y'))
        | otherwise = Forbidden (x,y)
           where x' = pred x
                 y' = succ y
                 primary | length fields <= y' || x == 0 = ' '
                         | otherwise = getFieldChar $ getFieldByCoord fields (x',y')
                 secondary = secondaryChar == '-'
                 secondary2 = secondaryChar2 == '|'
                 secondaryChar2 | length fields <= y' || length (fields !! y') <= x = ' '
                                | otherwise = getFieldChar $ getFieldByCoord fields (x,y')
                 secondaryChar | x == 0 = ' '
                               | otherwise = getFieldChar $ getFieldByCoord fields (x',y)

{-
    Primary:     Seondary:      Secondary2:
    \               \-              \
     \                              |
-}
getSouthEastMove :: (Int,Int) -> [[Field]] -> Bool -> Move
getSouthEastMove (x,y) fields includeSecondaryBranch
        | primary /= ' ' = Allowed (x',y')
        | includeSecondaryBranch && (secondary `xor` secondary2) = Allowed (getSecondary secondary (x',y) secondary2 (x,y'))
        | otherwise = Forbidden (x,y)
           where x' = succ x
                 y' = succ y
                 primary | length fields <= y' || length (fields !! y') <= x' = ' '
                         | otherwise = getFieldChar $ getFieldByCoord fields (x',y')
                 secondary = secondaryChar == '-'
                 secondary2 = secondaryChar2 == '|'
                 secondaryChar | length (fields !! y) <= x' = ' '
                               | otherwise = getFieldChar $ getFieldByCoord fields (x',y)
                 secondaryChar2 | length fields <= y' || (length (fields !! y') <= x) = ' '
                                | otherwise = getFieldChar $ getFieldByCoord fields (x,y')

getSecondary :: Bool -> (Int,Int) -> Bool -> (Int,Int) -> (Int,Int)
getSecondary secondary1 coord1 secondary2 coord2 
        | secondary1  = coord1
        | otherwise = coord2

getNewDirection :: Char -> Direction -> Direction
getNewDirection newChar oldDir
        | isJust newDir = fromJust newDir
        | otherwise = oldDir
    where dirMap = [((SouthEast,'-'),East),
                    ((SouthWest,'-'),West),
                    ((East,'/'),NorthEast),
                    ((West,'\\'),NorthWest),
                    ((East,'\\'),SouthEast),
                    ((West,'/'),SouthWest),
                    ((SouthEast,'|'),South),
                    ((NorthEast,'|'),North),
                    ((SouthWest,'|'),South),
                    ((NorthWest,'|'),North),
                    ((South,'/'),SouthWest),
                    ((South,'\\'),SouthEast),
                    ((North,'\\'),NorthWest),
                    ((North,'/'),NorthEast)]
          newDir = lookup (oldDir,newChar) dirMap

--end: Functions to decide how the train will move
xor :: Bool -> Bool -> Bool
xor a b = a /= b

goStep :: Move -> Function -> Direction -> (Move,Function,Direction)
goStep (Forbidden a) function dir  = (Forbidden a,function,dir)
goStep (Allowed (x,y)) function@(Function _ fields) dir = (nextCoord,visitedFunction,newDir)
            where visitedFunction = markAsVisited (x,y) function
                  nextCoord = getNextMove (x,y) dir visitedFunction True
                  newDir | isAllowed nextCoord = getNewDirection (getFieldChar(getFieldByCoord fields ( fromAllowed nextCoord))) dir
                         | otherwise = dir

exCode3 = ["$ 'main'"," \\","  \\","   \\----\\","         \\","         |","     #---/"]
exFunction3 = getFunction exCode3

exCode2 = ["$ 'main'"," \\ /----\\","  \\     |","   \\----/"]
exFunction2 = getFunction exCode2
getFunction code = e
    where (Programm (e:es)) = codeToProgramm code

