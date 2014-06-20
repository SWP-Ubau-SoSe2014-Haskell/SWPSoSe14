{- |
Module      :  SyntacticalAnalysis.hs
Description :  .
Maintainer  :  (c) Kristin Knorr, Marcus Hoffmann
License     :  MIT

Stability   :  stable

SyntacticalAnalysis receives output of Lexer and turns each rail function graph
into a list of paths. Each path is a triple and contains a Path-ID, a list of
lexemes and a Path-ID of the path that follows. Those lexemes are executed in
order and sequentially.
-}
module SyntacticalAnalysis (
                            process   -- main function of the module "SyntacticalAnalysis"
                           )
 where
 -- imports --
 import InterfaceDT as IDT
 import ErrorHandling as EH

 -- functions --
 process :: IDT.Lexer2SynAna -> IDT.SynAna2SemAna
 process (IDT.ILS input) = IDT.ISS output
  where
   output = map (\(x, y)->(x, pathes y (startNodes y))) input
 
 -- |generates all pathes of a graph
 pathes :: [IDT.LexNode] -> [Int] -> [(Int, [Lexeme], Int)]
 pathes xs ys = map (\x-> findPath x xs ys) ys
 
 -- |generates one path depending on initial node
 findPath :: Int -> [IDT.LexNode] -> [Int] -> (Int, [Lexeme], Int)
 findPath x xs ys = genPath x (generate x xs)
    where
        genPath :: Int -> [(Lexeme, Int)] -> (Int, [Lexeme], Int)
        genPath pathID leFoList = (pathID, map fst leFoList, (snd.last) leFoList)
        generate :: Int -> [IDT.LexNode] -> [(Lexeme, Int)]
        generate v = genElem . head . filter (\y -> fst' y == v)
        genElem :: IDT.LexNode -> [(Lexeme, Int)]
        genElem (nodeID, lex, fol)
            |elem fol ys || fol==0 = [(lex, fol)]
            |otherwise             = (lex, fol) : generate fol xs
 
 -- |generates a list of all nodes, which are needed to be initial nodes of path:
 -- 1 as functionstart; conditional jmp; indegree > 1
 startNodes :: [IDT.LexNode] -> [Int]
 startNodes [] = []
 startNodes xs = 1:[x | x <- [2..(length xs)], isJunct0 x xs || (inDeg x xs > 1) || isJunct1 x xs]
    where
        isJunct0 :: Int -> [IDT.LexNode] -> Bool
        isJunct0 x = any (\y -> Junction x == snd' y)
        isJunct1 :: Int -> [IDT.LexNode] -> Bool
        isJunct1 x = any (\y -> isJunct (snd' y) && x == trd' y)
        isJunct :: Lexeme -> Bool
        isJunct (Junction x) = True
        isJunct _ = False
        inDeg :: Int -> [IDT.LexNode] -> Int
        inDeg x = length . filter (\y-> trd' y==x)
 
 -- |fetch triple components
 fst' :: (a, b, c) -> a
 fst' (x, _, _) = x
 
 snd' :: (a, b, c) -> b
 snd' (_, x, _) = x
 
 trd' :: (a, b, c) -> c
 trd' (_, _, x) = x
