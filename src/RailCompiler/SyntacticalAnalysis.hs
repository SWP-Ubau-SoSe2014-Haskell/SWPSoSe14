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
 import Data.Maybe
 import qualified Data.Map as Map

 -- functions --
 process :: IDT.Lexer2SynAna -> IDT.SynAna2SemAna
 process (IDT.ILS input) = IDT.ISS output
  where
   output = map (\(x, y)->(x, pathes (mapify y Map.empty) (startNodes y))) input
 
 mapify :: [(Int, Lexeme, Int)] -> Map.Map Int (Lexeme, Int) -> Map.Map Int (Lexeme, Int)
 mapify [] map = map
 mapify ((id, lexeme, follower):xs) map = mapify xs (Map.insert id (lexeme, follower) map)

 -- |generates all pathes of a graph
 pathes :: Map.Map Int (Lexeme, Int) -> [Int] -> [(Int, [Lexeme], Int)]
 pathes _ [] = []
 pathes xs ys
  | Map.size xs == 0 = []
  | otherwise = map (\x-> findPath x xs ys) ys
 
 -- |generates one path depending on initial node
 findPath :: Int -> Map.Map Int (Lexeme, Int) -> [Int] -> (Int, [Lexeme], Int)
 findPath x xs ys = genPath x (generate x xs)
    where
        genPath :: Int -> [(Lexeme, Int)] -> (Int, [Lexeme], Int)
        genPath pathID leFoList = (pathID, map fst leFoList, (snd.last) leFoList)
        generate :: Int -> Map.Map Int (Lexeme, Int) -> [(Lexeme, Int)]
        generate v map = genElem $ fromJust $ Map.lookup v map
        genElem :: (Lexeme, Int) -> [(Lexeme, Int)]
        genElem (lex, fol)
            |elem fol ys || fol==0 = [(lex, fol)]
            |otherwise             = (lex, fol) : generate fol xs
 
 -- |fetch triple components
 fst' :: (a, b, c) -> a
 fst' (x, _, _) = x
 
 -- |generates a list of all nodes, which are needed to be initial nodes of path:
 -- 1 as functionstart; conditional jmp; indegree > 1
 startNodes :: [IDT.LexNode] -> [Int]
-- startNodes xs = error (show (Map.toList (nodeCount xs Map.empty)))
-- startNodes xs = error (show (fst' (head xs):(Map.keys $ Map.filter (/= 1) $ nodeCount xs Map.empty)))
 startNodes [] = []
 startNodes xs = fst' (head xs):filter (/=0) (Map.keys $ Map.filter (/= 1) $ nodeCount xs Map.empty)
    where
        -- result type: [(ID, Count)]
        nodeCount :: [IDT.LexNode] -> Map.Map Int Int -> Map.Map Int Int
        nodeCount [] res = res
        -- we WANT junctions to be at the very end
        nodeCount ((_, Junction x, y):xs) res = nodeCount xs $ incCount (incCount res x 2) y 2
        nodeCount ((_, Lambda x, y):xs) res = nodeCount xs $ incCount (incCount res x 2) y 1
        nodeCount ((_, _, y):xs) res = nodeCount xs $ incCount res y 1
        -- update the count
        incCount :: Map.Map Int Int -> Int -> Int-> Map.Map Int Int
        incCount map id count
            | Map.member id map = Map.adjust (count +) id map
            | otherwise = Map.insert id count map
