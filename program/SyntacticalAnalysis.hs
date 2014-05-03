module SyntacticalAnalysis (
                            process   -- main function of the module "SyntacticalAnalysis"
					       )
 where
 
 -- imports --
 import InterfaceDT as IDT
 
 -- functions --
 process :: IDT.Lexer2SynAna -> IDT.SynAna2SemAna
 process (IDT.ILS input) = (IDT.ISS output)
  where
   output = map (\(x, y)->(x, (pathes y (startNodes y)))) input
 
 -- |generates all pathes of a graph
 pathes :: [IDT.LexNode] -> [Int] -> [(Int, [Lexeme], Int)]
 pathes xs ys = map (\x-> (findPath x xs ys)) ys
 
 -- |generates one path depending on initial node
 findPath :: Int -> [IDT.LexNode] -> [Int] -> (Int, [Lexeme], Int)
 findPath x xs ys = genPath x (generate x xs)
    where
        genPath pathID leFoList = (pathID, (map fst leFoList), ((snd.last) leFoList))
        generate v = genElem.head.(filter (\y->(fst' y)==v))
        genElem (nodeID, lex, fol)
            |elem fol ys || fol==0 = [(lex, fol)]
            |otherwise             = (lex, fol):(generate fol xs)
 
 -- |generates a list of all nodes, which are needed to be initial nodes of path:
 -- 1 as functionstart; conditional jmp; indegree > 1
 startNodes :: [IDT.LexNode] -> [Int]
 startNodes xs = 1:[x| x <- [2..(length xs)] , (isJunct x xs) || ((length.(filter (\y-> (trd' y)==x))) xs)>1]
    where
        isJunct :: Int -> [IDT.LexNode] -> Bool
        isJunct _ []     = False
        isJunct x ((_, Junction y, _):ys)
            |x==y        = True
            |otherwise   = isJunct x ys
        isJunct x (y:ys) = isJunct x ys
 
 -- |fetch triple components
 fst' :: (a, b, c) -> a
 fst' (x, _, _) = x
 
 snd' :: (a, b, c) -> b
 snd' (_, x, _) = x
 
 trd' :: (a, b, c) -> c
 trd' (_, _, x) = x
 
