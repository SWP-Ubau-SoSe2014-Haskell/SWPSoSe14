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
              step, parse, IP(IP), posx, posy, start, crash, turnaround, junctionturns, lambdadirs , move , current, RelDirection(Forward)
             )
 where

 -- imports --
 import InterfaceDT as IDT
 import ErrorHandling as EH
 import Data.List
 import Text.Printf
 import Data.Maybe
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
   | otherwise = (funcname firstline, finalize (head nxs))
  where
    emptyfunc = (funcname firstline, [(1, Start, 0)])
    firstline = fromJust (Map.lookup 0 code)
    (nxs, _) = nodes code [[(1, Start, 0)]] start

 -- |Get the name of the given function.
 funcname :: Map.Map Int Char -- ^A line containing the function declaration,
                              -- e. g. @$ \'main\'@.
    -> String -- ^The function name
 funcname line = helper (tostring 0 line)
  where
   tostring i line
     | isNothing (Map.lookup i line) = ""
     | otherwise = fromJust (Map.lookup i line):tostring (i+1) line
   helper line
     | null line || length (elemIndices '\'' line) < 2 || null fn = error EH.strFunctionNameMissing
     | not $ null $ fn `intersect` "'{}()!" = error EH.strInvalidFuncName
     | otherwise = fn
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
      newnode = (nodecount ip) + 1
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
     newnode = (nodecount ip) + 1
     newlist = (newnode, lexeme, 0) `prepend` update list (path ip) newnode
     newip = nodeadd ip newnode
     prepend newx (x:xs) = (newx:x):xs
     isattributed (Junction _) = True
     isattributed (Lambda _) = True
     isattributed _ = False
     (final, finip) = nodes code ([]:tempnodes) (ipmerge trueip tempip)
     (tempnodes, tempip) = nodes code ([]:newlist) (ipmerge falseip newip)
     (falseip, trueip) = if current code ip == '&' then lambdadirs ip else junctionturns code ip
 

 -- |Shift a node by the given amount. May be positive or negative.
 -- This is used by 'toGraph' and 'fromGraph' to shift all nodes by 1 or -1, respectively,
 -- which is done because the portable text representation of the graph does not include
 -- a leading "Start" node with ID 1 -- instead, the node with ID 1 is the first "real"
 -- graph node. In other words, when exporting to the text representation, the "Start"
 -- node is removed and all other nodes are "shifted" by -1 using this function. When
 -- importing, a "Start" node is added and all nodes are shifted by 1.
 offset :: Int -- ^Amount to shift node by.
    -> IDT.LexNode -- ^Node to operate on.
    -> IDT.LexNode -- ^Shifted node.
 offset c (node, lexeme, 0) = (node + c, lexeme, 0)
 offset c (node, lexeme, following) = (node + c, lexeme, following + c)

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

 -- |Collect characters until a condition is met while moving in the current direction.
 stepwhile :: IDT.Grid2D -- ^Line representation of current function.
    -> IP -- ^Current instruction pointer.
    -> (Char -> Bool) -- ^Function: Should return True if collection should stop.
                      -- Gets the current Char as an argument.
    -> (String, IP) -- ^Collected characters and the new instruction pointer.
 stepwhile code ip fn
   | not (fn curchar) = ("", ip)
   | not (moveable code ip Forward) = error EH.strMissingClosingBracket
   | curchar `elem` "'{}()" = error EH.strInvalidVarName
   | otherwise = (curchar:resstring, resip)
  where
   curchar = current code ip
   (resstring, resip) = stepwhile code (move ip Forward) fn

 -- |Checks if the instruction pointer can be moved without leaving the grid
 moveable :: IDT.Grid2D -- ^Line representation of current function
    -> IP -- ^Current instruction pointer
    -> RelDirection -- ^Where to move to
    -> Bool -- ^Whether or not the move could be made
 moveable code ip reldir
   | Map.size code == 0 = False
   | newy < 0 || newy >= Map.size code = False
   | dir ip `elem` [W, E] && (newx < 0 || newx >= Map.size line) = False
   | otherwise = True
  where
   (newy, newx) = posdir ip reldir
   line = fromJust (Map.lookup newy code)

 -- |Read a string constant and handle escape sequences like \n.
 -- Raises an error on invalid escape sequences and badly formatted constants.
 readconstant :: IDT.Grid2D -- ^Current function in line representation
    -> IP -- ^Current instruction pointer
    -> Char -- ^Opening string delimiter, e. g. '['
    -> Char -- ^Closing string delimiter, e. g. ']'
    -> (String, IP) -- ^The processed constant and the new instruction pointer
 readconstant code ip startchar endchar
    | curchar == startchar  = error EH.strNestedOpenBracket
    | curchar == endchar    = ("", ip)
    | not (moveable code ip Forward) = error EH.strMissingClosingBracket
    | otherwise             = (newchar:resstring, resip)
  where
    curchar                 = current code ip
    (newchar, newip)        = processescape
    (resstring, resip)      = readconstant code newip startchar endchar

    -- This does the actual work and converts the escape sequence
    -- (if there is no escape sequence at the current position, do
    -- nothing and pass the current Char through).
    processescape :: (Char, IP)
    processescape
        | curchar /= '\\'   = (curchar, move ip Forward)
        | esctrail /= '\\'  = error EH.strNonSymmetricEscape
        | otherwise         = case escsym of
            '\\' -> ('\\', escip)
            '['  -> ('[', escip)
            ']'  -> (']', escip)
            'n'  -> ('\n', escip)
            't'  -> ('\t', escip)
            _    -> error $ printf EH.strUnhandledEscape escsym
      where
        [escsym, esctrail]  = lookahead code ip 2
        -- Points to the character after the trailing backslash
        escip               = skip code ip 3

 -- |Lookahead n characters in the current direction.
 lookahead :: IDT.Grid2D -- ^Line representation of current function
    -> IP -- ^Current instruction pointer
    -> Int -- ^How many characters of lookahead to produce?
    -> String -- ^n characters of lookahead
 lookahead code ip 0 = []
 lookahead code ip n = current code newip : lookahead code newip (n-1)
  where
    newip = move ip Forward

 -- |Skip n characters in the current direction and return the new IP.
 skip :: IDT.Grid2D -- ^Line representation of current function
    -> IP -- ^Current instruction pointer
    -> Int  -- ^How many characters to skip? If 1, this is the same
            -- as doing "move ip Forward".
    -> IP -- ^New instruction pointer
 skip code ip n = foldl (\x _ -> move x Forward) ip [1..n]

 -- |Get the 'Char' at the next position of the instruction pointer
 next :: IDT.Grid2D -> IP -> Char
 next code ip = current code $ move ip Forward

 -- |Get adjacent (left secondary, primary, right secondary)
 -- symbols for the current IP position.
 adjacent :: IDT.Grid2D -- ^Line representation of the current function.
     -> IP -- ^Current instruction pointer.
     -> (Char, Char, Char) -- ^Adjacent (left secondary, primary, right secondary) symbols
 adjacent code ip
  | current code ip `elem` turnblocked = (' ', charat code (posdir ip Forward), ' ')
  | otherwise = (charat code (posdir ip InstructionPointer.Left), charat code (posdir ip Forward), charat code (posdir ip InstructionPointer.Right))

 -- |Get the next lexeme at the current position.
 parse :: IDT.Grid2D -- ^Line representation of current function.
    -> IP -- ^Current instruction pointer.
    -> (Maybe IDT.Lexeme, IP) -- ^Resulting lexeme (if any) and
                              -- the new instruction pointer.
 parse code ip = junctioncheck $ case current code ip of
   'b' -> (Just Boom, ip)
   'e' -> (Just EOF, ip)
   'i' -> (Just Input, ip)
   'o' -> (Just Output, ip)
   'u' -> (Just Underflow, ip)
   '?' -> (Just RType, ip)
   'a' -> (Just Add1, ip)
   'd' -> (Just Divide, ip)
   'm' -> (Just Multiply, ip)
   'r' -> (Just Remainder, ip)
   's' -> (Just Subtract, ip)
   '0' -> (Just (Constant "0"), ip)
   '1' -> (Just (Constant "1"), ip)
   '2' -> (Just (Constant "2"), ip)
   '3' -> (Just (Constant "3"), ip)
   '4' -> (Just (Constant "4"), ip)
   '5' -> (Just (Constant "5"), ip)
   '6' -> (Just (Constant "6"), ip)
   '7' -> (Just (Constant "7"), ip)
   '8' -> (Just (Constant "8"), ip)
   '9' -> (Just (Constant "9"), ip)
   'c' -> (Just Cut, ip)
   'p' -> (Just Append, ip)
   'z' -> (Just Size, ip)
   'n' -> (Just Nil, ip)
   ':' -> (Just Cons, ip)
   '~' -> (Just Breakup, ip)
   'f' -> (Just (Constant "0"), ip)
   't' -> (Just (Constant "1"), ip)
   'g' -> (Just Greater, ip)
   'q' -> (Just Equal, ip)
   '$' -> (Just Start, ip)
   '#' -> (Just Finish, ip)
   '.' -> (Just NOP, ip)
   'v' -> (Just (Junction 0), ip)
   '^' -> (Just (Junction 0), ip)
   '>' -> (Just (Junction 0), ip)
   '<' -> (Just (Junction 0), ip)
   '&' -> (Just (Lambda 0), ip)
   '[' -> let (string, newip) = readconstant code tempip '[' ']' in (Just (Constant string), newip)
   ']' -> let (string, newip) = readconstant code tempip ']' '[' in (Just (Constant string), newip)
   '{' -> let (string, newip) = stepwhile code tempip (/= '}') in (Just (Call $ checkstring string), newip)
   '}' -> let (string, newip) = stepwhile code tempip (/= '{') in (Just (Call $ checkstring string), newip)
   '(' -> let (string, newip) = stepwhile code tempip (/= ')') in (pushpop string, newip)
   ')' -> let (string, newip) = stepwhile code tempip (/= '(') in (pushpop string, newip)
   _ -> (Nothing, turn (current code ip) ip)
  where
   junctioncheck (Nothing, ip)
     | current code ip `elem` "+x*" && next code ip `elem` "v^<>" = (Nothing, crashfrom ip)
     | forward == ' ' && (left == current code ip || right == current code ip) = (Nothing, crashfrom ip)
     | forward == ' ' && (left `elem` "v^<>+x*" || right `elem` "v^<>+x*") = (Nothing, crashfrom ip)
     | otherwise = (Nothing, ip)
    where
     (left, forward, right) = adjacent code ip
   junctioncheck (lexeme, ip)
    | next code ip `elem` "v^<>" = (lexeme, crashfrom ip)
    | otherwise = (lexeme, ip)
   tempip = move ip Forward
   checkstring string = if '!' `elem` string then error EH.strInvalidVarName else string
   pushpop string
    | length string < 2 = Just (Push $ checkstring string)
    | head string == '!' && last string == '!' = Just (Pop (checkstring $ tail $ init string))
		| otherwise = Just (Push $ checkstring string)

 -- |Brings it into right order
 finalize :: [IDT.LexNode] -- ^'LexNode's to convert.
    -> [IDT.LexNode] -- ^Resulting list of 'IDT.LexNode's.
 finalize = reverse

 -- |Return valid chars for movement depending on the current direction.
 valids :: IDT.Grid2D -- ^Line representation of current function.
    -> IP -- ^Current instruction pointer.
    -> (String, String, String) -- ^Tuple consisting of:
                                --
                                --     * Valid characters for movement to the (relative) left.
                                --     * Valid characters for movement in the (relative) forward direction.
                                --     * Valid characters for movement to the (relative) right.
 valids code ip = tripleinvert (commandchars ++ dirinvalid ip ++ finvalid ip{dir = absolute ip InstructionPointer.Left}, finvalid ip, commandchars ++ dirinvalid ip ++ finvalid ip{dir = absolute ip InstructionPointer.Right})
  where
   tripleinvert (l, f, r) = (filter (`notElem` l) everything, filter (`notElem` f) everything, filter (`notElem` r) everything)
   finvalid ip = dirinvalid ip ++ crossinvalid ip -- illegal to move forward
   dirinvalid ip -- illegal without crosses
    | dir ip `elem` [E, W] = "|"
    | dir ip `elem` [NE, SW] = "\\"
    | dir ip `elem` [N, S] = "-"
    | dir ip `elem` [NW, SE] = "/"
    | otherwise = ""
   crossinvalid ip -- illegal crosses
    | dir ip `elem` [N, E, S, W] = "x"
    | otherwise = "+"
   cur = current code ip
   everything = "+\\/x|-" ++ always
   always = "^v<>*@{}[]()" ++ commandchars
 
 -- list of chars that are commands in rail
 commandchars :: String
 commandchars = "abcdefgimnopqrstuz:~0123456789?#&"

 -- list of chars which do not allow any turning
 turnblocked :: String
 turnblocked = "$*+x" ++ commandchars

 -- |Convert a graph/AST into a portable text representation.
 -- See also 'fromGraph'.
 fromAST :: IDT.Lexer2SynAna -- ^Input graph/AST/forest.
    -> String -- ^Portable text representation of the AST:
              --
              -- Each function is represented by its own section. A section has a header
              -- and content; it continues either until the next section, a blank line or
              -- the end of the file, whichever comes first.
              --
              -- A section header consists of a single line containing the name of the function,
              -- enclosed in square brackets, e. g. @[function_name]@. There cannot be any whitespace
              -- before the opening bracket.
              --
              -- The section content consists of zero or more non-blank lines containing exactly
              -- three records delimited by a semicolon @;@. Each line describes a node and contains
              -- the following records, in this order:
              --
              --     * The node ID (numeric), e. g. @1@.
              --     * The Rail lexeme, e. g. @o@ or @[constant]@ etc. Note that track lexemes like
              --     @-@ or @+@ are not included in the graph. Multi-character lexemes like constants
              --     may include semicolons, so you need to parse them correctly! In other words, you need
              --     to take care of lines like @1;[some ; constant];2@.
              --     * Node ID of the follower node, e. g. @2@. May be @0@ if there is no next node.
 fromAST (IDT.ILS graph) = unlines $ map fromGraph graph

 -- |Convert a portable text representation of a graph into a concrete graph representation.
 -- See also 'toGraph'. See 'fromAST' for a specification of the portable text representation.
 toAST :: String -- ^Portable text representation. See 'fromAST'.
    -> IDT.Lexer2SynAna -- ^Output graph.
 toAST input = IDT.ILS (map toGraph $ splitfunctions input)

 -- |Convert an 'IDT.Graph' for a single function to a portable text representation.
 -- See 'fromAST' for a specification of the representation.
 --
 -- TODO: Currently, this apparently crashes the program on invalid input. More sensible error handling?
 --       At least a nice error message would be nice.
 fromGraph :: IDT.Graph -- ^Input graph.
    -> String -- ^Text representation.
 fromGraph (funcname, nodes) = unlines $ ("["++funcname++"]"):tail (map (fromLexNode . offset (-1)) nodes)
  where
   fromLexNode :: IDT.LexNode -> String
   fromLexNode (id, lexeme, follower) = show id ++ ";" ++ fromLexeme lexeme ++ ";" ++ show follower ++ optional lexeme
   fromLexeme :: IDT.Lexeme -> String
   fromLexeme Boom = "b"
   fromLexeme EOF = "e"
   fromLexeme Input = "i"
   fromLexeme Output = "o"
   fromLexeme Underflow = "u"
   fromLexeme RType = "?"
   fromLexeme (Constant string) = "["++string++"]"
   fromLexeme (Push string) = "("++string++")"
   fromLexeme (Pop string) = "(!"++string++"!)"
   fromLexeme (Call string) = "{"++string++"}"
   fromLexeme Add1 = "a"
   fromLexeme Divide = "d"
   fromLexeme Multiply = "m"
   fromLexeme Remainder = "r"
   fromLexeme Subtract = "s"
   fromLexeme Cut = "c"
   fromLexeme Append = "p"
   fromLexeme Size = "z"
   fromLexeme Nil = "n"
   fromLexeme Cons = ":"
   fromLexeme Breakup = "~"
   fromLexeme Greater = "g"
   fromLexeme Equal = "q"
   fromLexeme Start = "$"
   fromLexeme Finish = "#"
   fromLexeme (Junction _) = "v"
   fromLexeme (Lambda _) = "&"
   fromLexeme NOP = "."
   optional (Junction follow) = ';' : show follow
   optional (Lambda follow) = ';' : show follow
   optional _ = ";0"

 -- |Split a portable text representation of multiple function graphs (a forest) into separate
 -- text representations of each function graph.
 splitfunctions :: String -- ^Portable text representation of the forest.
    -> [[String]] -- ^List of lists, each being a list of lines making up a separate function graph.
 splitfunctions = groupBy (\_ y -> null y || head y /= '[') . filter (not . null) . lines

 -- |Convert a portable text representation of a single function into an 'IDT.Graph'.
 -- Raises 'error's on invalid input (see 'ErrorHandling').
 toGraph :: [String] -- ^List of lines making up the text representation of the function.
    -> IDT.Graph -- ^Graph describing the function.
 toGraph lns = (init $ tail $ head lns, (1, Start, 2):map (offset 1) (nodes $ tail lns))
  where
   nodes [] = []
   nodes (ln:lns) = (read id, fixedlex, read follower):nodes lns
    where
     (id, other) = span (/=';') ln
     (lex, ip) = parse (convert [other]) (IP 0 1 0 E Forward Map.empty)
     (follower, attribute) = span (/=';') (drop (2 + posx ip) other)
     fixedlex
      | isJunction lex = Junction (read $ tail attribute)
      | isLambda lex = Lambda (read $ tail attribute)
      | otherwise = fromJust lex
     fromJust Nothing = error $ printf EH.shrLineNoLexeme ln
     fromJust (Just x) = x
     isJunction (Just (Junction _)) = True
     isJunction _ = False
     isLambda (Just (Lambda _)) = True
     isLambda _ = False
     convert code = Map.fromList $ zip [0..] (map (Map.fromList . zip [0..]) code)

-- vim:ts=2 sw=2 et
