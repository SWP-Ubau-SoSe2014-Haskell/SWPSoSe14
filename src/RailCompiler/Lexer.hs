{- |
Module      : Lexer
Description : Processes preprocessor output into input for the syntactical analysis.
Maintainer  : Christian H. et al.
License     : MIT

The lexer receives the output of the preprocessor -- a list of lists of strings,
where each list represents a single function -- and turns it into a forest of function
graphs. The forest is represented as a list of tuples; each tuple describes a function
graph and contains the function name as a string as well as the function's graph itself.

A single function graph is a list of nodes. A node is a triple containing the following
elements, in this order:

    * Node ID as an Integer. Starts with 1 for each function.
    * The 'IDT.Lexeme' for this node.
    * The node ID of the follower node or 0 if there is no next node.

Note that for valid input, the only node with a follower ID of 0 can be
a node containing the 'IDT.Finish' lexeme. If any other node contains a follower
ID of 0, this is an error (or, in Rail terms, a "crash".
-}
module Lexer (
              -- * Main (pipeline) functions
              process,
              -- * Utility functions
              fromAST, toAST,
              -- * Editor functions
              step, parse, IP(IP), posx, posy, count, start, crash, turnaround, junctionturns, lambdadirs , move , current, RelDirection(Forward), funcname
             )
 where

 -- imports --
 import InterfaceDT as IDT
 import ErrorHandling as EH
 import Data.Maybe
 import Data.List
 import AST
 import InstructionPointer
 import qualified Data.Map as Map
 
 -- functions --

 -- |Process preprocessor output into a list of function ASTs.
 --
 --
 -- Raises 'error's on invalid input; see 'ErrorHandling' for a list of error messages.
 process :: IDT.PreProc2Lexer -- ^Preprocessor output (a list of lists of strings; i. e. a list of functions
                              -- in their line representation).
    -> IDT.Lexer2SynAna -- ^A list of ASTs, each describing a single function.
 process (IDT.IPL input) = IDT.ILS $ map (\(x, _) -> processfn x) input

 -- |Process a single function.
 processfn :: IDT.Grid2D -- ^The lines representing the function.
    -> IDT.Graph -- ^A graph of nodes representing the function.
                   -- There may be more functions because of lambdas.
 processfn code
   | Map.size code < 2 = emptyfunc -- oneliners are illegal; follower == 0 will
                               -- lead to a crash, which is what we want.
   | Map.size firstline == 0 || fromJust (Map.lookup 0 firstline) /= '$' = emptyfunc
   | otherwise = (func, finalize (head nxs))
  where
    emptyfunc = (func, [(1, Start, 0)])
    firstline = fromJust (Map.lookup 0 code)
    func = case funcname code of
      (Prelude.Left name) -> name
      (Prelude.Right err) -> error err
    (nxs, _) = nodes code [[(1, Start, 0)]] start

 -- |Get the name of the given function.
 funcname :: IDT.Grid2D -> Either String String
 funcname code
   | isNothing (Map.lookup 0 code) = Prelude.Right EH.strFunctionNameMissing
   | otherwise = helper (tostring 0 line)
  where
   line = fromJust (Map.lookup 0 code)
   tostring i line
     | isNothing (Map.lookup i line) = if i > Map.size line then "" else tostring (i+1) line
     | otherwise = fromJust (Map.lookup i line):tostring (i+1) line
   helper line
     | null line || length (elemIndices '\'' line) < 2 || null fn = Prelude.Right EH.strFunctionNameMissing
     | not $ null $ fn `intersect` "'{}()!" = Prelude.Right EH.strInvalidFuncName
     | otherwise = Prelude.Left fn
    where fn = takeWhile (/='\'') $ tail $ dropWhile (/='\'') line

 -- |Get the nodes for the given function.
 nodes :: IDT.Grid2D  -- ^Lines representing the function.
    -> [[IDT.LexNode]] -- ^Current graph representing the function.
                      -- Initialize with @[[(1, Start, 0, (0, 0, SE))]]@.
    -> IP -- ^Current instruction pointer.
          -- Initialize with @'start'@.
    -> ([[IDT.LexNode]], IP) -- ^Final graph for the function and the new instruction pointer.
 nodes code list ip
  | current code tempip == ' ' = (list, tempip) -- If we are not finished yet, this will
                                                -- automatically lead to a
                                                -- crash since the list will have
                                                -- a leading node without a follower
                                                -- (follower == 0) because it is
                                                -- not modified here at all.
  | endless = (endlesslist, crashfrom ip)
  | otherwise = nodes code newlist newip
   where
      -- This checks if we have e. g. two reflectors that "bounce" the IP between them endlessly.
      endless = count ip > 8 * Map.size (fromJust (Map.lookup 0 code)) * Map.size (fromJust (Map.lookup 0 code))
      endlesslist = (newnode, NOP, newnode) `prepend` update list (path ip) newnode
      newnode = nodecount ip + 1
      prepend newx (x:xs) = (newx:x):xs
      tempip = step code ip
      (newlist, newip) = handle code list tempip

 -- |Helper function for 'nodes': Handle the creation of the next 'PreLexNode'
 -- for the current function.
 handle :: IDT.Grid2D -- ^Line representation of input function.
    -> [[IDT.LexNode]] -- ^Current list of nodes.
    -> IP -- ^Current instruction pointer.
    -> ([[IDT.LexNode]], IP) -- ^New node list and new instruction pointer.
 handle code list ip = helper code list newip lexeme
  where
   (lexeme, newip) = parse code ip
   helper _ list ip Nothing = (list, ip)
   helper code list ip (Just lexeme)
     | knownat > 0 = (update list (path ip) knownat, crashfrom ip)
     | lexeme == Finish = (newlist, crashfrom newip)
     | isattributed lexeme = (merge final, crashfrom finip)
     | otherwise = (newlist, newip)
    where
     knownat = visited ip
     newnode = nodecount ip + 1
     newlist = (newnode, lexeme, 0) `prepend` update list (path ip) newnode
     newip = nodeadd ip newnode
     prepend newx (x:xs) = (newx:x):xs
     isattributed (Junction _) = True
     isattributed (Lambda _) = True
     isattributed _ = False
     (final, finip) = nodes code ([]:tempnodes) (ipmerge trueip tempip)
     (tempnodes, tempip) = nodes code ([]:newlist) (ipmerge falseip newip)
     (falseip, trueip) = if current code ip == '&' then lambdadirs ip else junctionturns code ip
 
 -- |Change the following node of the first (i. e. "last", since the list is reversed)
 -- node in the graph.
 update :: [[IDT.LexNode]] -- ^The graph to operate on.
    -> RelDirection --  ^Turn taken on last Junctions
    -> Int -- ^ID of new follower to set for the first node in the list.
    -> [[IDT.LexNode]] -- ^Resulting graph.
 update list@(x:xs) dir following
  | null x && startsattributed xs && dir == InstructionPointer.Left = helpera list following
  | null x && not (null xs) && startsattributed (tail xs) && dir == InstructionPointer.Right = x:head xs:helper (head (tail xs)) following:tail (tail xs)
  | null x = list
  | otherwise = helper x following:xs
   where
    helper ((node, lexeme, _):xs) following = (node, lexeme, following):xs
    helpera (x:(((node, Junction _, following):xs):xss)) attribute = x:(((node, Junction attribute, following):xs):xss)
    helpera (x:(((node, Lambda _, following):xs):xss)) attribute = x:(((node, Lambda attribute, following):xs):xss)
    startsattributed (((_, Junction _, _):_):_) = True
    startsattributed (((_, Lambda _, _):_):_) = True
    startsattributed _ = False

 -- merges splitted graphs (e.g. Junction)
 -- x3 is the graph until the special node appeared
 -- x2 is the graph that will result in the special attribute
 -- x1 is the graph that will become the follower
 merge :: [[IDT.LexNode]] -> [[IDT.LexNode]]
 merge (x1:x2:x3:xs) = (x1 ++ x2 ++ x3):xs

 -- |Move the instruction pointer a single step.
 step :: IDT.Grid2D -- ^Current function in its line representation.
    -> IP -- ^Current instruction pointer.
    -> IP -- ^New instruction pointer.
 step code ip
   | forward `elem` fval = move ip Forward
   | left `elem` lval && right `elem` rval = crashfrom ip
   | left `elem` lval = move ip InstructionPointer.Left
   | right `elem` rval = move ip InstructionPointer.Right
   | otherwise = crashfrom ip
  where
   (left, forward, right) = adjacent code ip
   (lval, fval, rval) = valids code ip

 -- |Brings it into right order
 finalize :: [IDT.LexNode] -- ^'LexNode's to convert.
    -> [IDT.LexNode] -- ^Resulting list of 'IDT.LexNode's.
 finalize = reverse

-- vim:ts=2 sw=2 et
