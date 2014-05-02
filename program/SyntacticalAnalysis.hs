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
   output= map (\(x,y)->(x,(pathes y (pathStartNodes y)))) input --ILS [(String,[LexNode])]

   
 pathes::[IDT.LexNode]->[Int]->[(Int, [Lexem], Int)]
 pathes xs ys = map (\x-> (findPath x xs ys)) ys
   
 findPath:: Int -> [IDT.LexNode] -> [Int] -> (Int,[Lexem],Int)
 findPath x xs ys= (x , (reverse (fst var)) , (snd var))
                where
                    var= lexemList x xs ys
                    lexemList x xs ys = help x xs ys ([],0)
                           where
                               help x xs ys (zs,z)
                                   |elem fol ys || fol==0 = (lex:zs,fol)
                                   |otherwise = help fol xs ys ((lex:zs),z)
                                       where
                                           xTrip = head(filter (\y->(fst' y)==x) xs)
                                           lex   = snd' xTrip
                                           fol   = trd xTrip


 fst'::(a,b,c)->a
 fst' (x,_,_)=x
 
 snd'::(a,b,c)->b
 snd' (_,x,_)=x
 
 trd::(a,b,c)->c
 trd (_,_,x) = x

-- generates a list of nodes, which are starting nodes of pathes
 pathStartNodes:: [IDT.LexNode] -> [Int]
 pathStartNodes xs = 1:(filter (/= 0) (pathStartNodes' xs [] []))

-- generates a list of nodes with indegree > 1 or junction
 pathStartNodes':: [IDT.LexNode] -> [Int] -> [Int] -> [Int]
 pathStartNodes' [] seen out = out
 pathStartNodes' ((_, Junction x , y ):xs) seen out
    |yInOut && xInOut = pathStartNodes' xs seen out
    |xInOut && yInSeen = pathStartNodes' xs seen (y:out)
    |xInOut = pathStartNodes' xs (y:seen) out
    |equal || yInOut = pathStartNodes' xs seen (x:out)
    |yInSeen = pathStartNodes' xs seen (x:(y:out))
    |otherwise = pathStartNodes' xs (y:seen) (x:out)
        where
            equal   = (x==y)
            xInOut  = elem x out
            yInOut  = elem y out
            yInSeen = elem y seen
 pathStartNodes' ((_, _ , y):xs) seen out
    |elem y out = pathStartNodes' xs seen out
    |elem y seen = pathStartNodes' xs seen (y:out)
    |otherwise = pathStartNodes' xs (y:seen) out



