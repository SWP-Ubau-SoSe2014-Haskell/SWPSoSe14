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

 -- |Modified 'IDT.LexNode' with an additional identifier for nodes
 -- to check whether we have circles in the graph.
 --
 -- The identifier is the last element of the tuple and contains
 -- the following sub-elements, in this order:
 --
 --     * When we visited this node, at which X position did we start to parse its lexeme?
 --     * When we visited this node, at which Y position did we start to parse its lexeme?
 --     * When we visited this node, from which direction did we come?
 type PreLexNode = (Int, IDT.Lexeme, Int, (Int, Int, Direction))
 -- |An absolute direction.
 data Direction = N | NE | E | SE | S | SW | W | NW deriving (Eq, Show)
 -- |A relative direction.
 data RelDirection = Left | Forward | Right deriving (Eq, Show)
 -- |Instruction pointer consisting of position and an orientation.
 data IP =
    IP {
      -- |Number of processed characters since start of current function.
      count :: Int,
      -- |Current X position.
      posx :: Int,
      -- |Current Y position.
      posy :: Int,
      -- |Current 'Direction'.
      dir :: Direction,
			-- |Determines if the instruction pointer is on a left or right path of a Junction
			path :: RelDirection
    }
  deriving (Show)

 instance Eq IP
  where
   (==) ipl ipr = posx ipl == posx ipr && posy ipl == posy ipr && dir ipl == dir ipr
 
 -- functions --

 -- |Process preprocessor output into a list of function ASTs.
 --
 --
 -- Raises 'error's on invalid input; see 'ErrorHandling' for a list of error messages.
 process :: IDT.PreProc2Lexer -- ^Preprocessor output (a list of lists of strings; i. e. a list of functions
                              -- in their line representation).
    -> IDT.Lexer2SynAna -- ^A list of ASTs, each describing a single function.
 process (IDT.IPL input) = IDT.ILS $ map processfn input

 -- |Process a single function.
 processfn :: IDT.Grid2D -- ^The lines representing the function.
    -> IDT.Graph -- ^A graph of nodes representing the function.
                   -- There may be more functions because of lambdas.
 processfn [x] = (funcname x, [(1, Start, 0)]) -- oneliners are illegal; follower == 0 will
                                                 -- lead to a crash, which is what we want.
 processfn code@(x:xs) = if head x /= '$' then (funcname x, [(1, Start, 0)]) else (funcname x, finalize (head nxs) [])
  where
    (nxs, _) = nodes code [[(1, Start, 0, (0, 0, SE))]] start

 -- |Get the name of the given function.
 --
 -- TODO: Note that this will crash the entire program if there is
 -- no function name.
 funcname :: String -- ^A line containing the function declaration,
                    -- e. g. @$ \'main\'@.
    -> String -- ^The function name.
 funcname line
   | null line || length (elemIndices '\'' line) < 2 = error EH.strFunctionNameMissing
   | not $ null $ fn `intersect` "'{}()!" = error EH.strInvalidFuncName
   | otherwise = fn
  where fn = takeWhile (/='\'') $ tail $ dropWhile (/='\'') line

 -- |Get the nodes for the given function.
 nodes :: IDT.Grid2D  -- ^Lines representing the function.
    -> [[PreLexNode]] -- ^Current graph representing the function.
                      -- Initialize with @[[(1, Start, 0, (0, 0, SE))]]@.
    -> IP -- ^Current instruction pointer.
          -- Initialize with @'start'@.
    -> ([[PreLexNode]], IP) -- ^Final graph for the function and the new instruction pointer.
 nodes code list ip
  | current code tempip == ' ' = (list, tempip) -- If we are not finished yet, this will
                                                -- automatically lead to a
                                                -- crash since the list will have
                                                -- a leading node without a follower
                                                -- (follower == 0) because it is
                                                -- not modified here at all.
  | otherwise = if endless then (endlesslist, crash) else nodes code newlist newip
     where
      -- This checks if we have e. g. two reflectors that "bounce" the IP between them
      -- endlessly.
      endless = count ip > sum (map length code)
      endlesslist = (newnode, NOP, newnode, (-1, -1, SE)) `prepend` update list (path ip) newnode
      newnode = sum (map length list) + 1
      prepend newx (x:xs) = (newx:x):xs
      tempip = step code ip
      (newlist, newip) = handle code list tempip

 -- |Helper function for 'nodes': Handle the creation of the next 'PreLexNode'
 -- for the current function.
 handle :: IDT.Grid2D -- ^Line representation of input function.
    -> [[PreLexNode]] -- ^Current list of nodes.
    -> IP -- ^Current instruction pointer.
    -> ([[PreLexNode]], IP) -- ^New node list and new instruction pointer.
 handle code list ip = helper code list newip lexeme
  where
   (lexeme, newip) = parse code ip
   helper _ list ip Nothing = (list, ip)
   helper code list ip (Just lexeme)
     | knownat > 0 = (update list (path ip) knownat, crash)
     | lexeme == Finish = (newlist, crash)
     | isattributed lexeme = (merge final, crash)
     | otherwise = (newlist, ip{count = 0})
    where
     knownat = visited list ip
     newnode = sum (map length list) + 1
     newlist = (newnode, lexeme, 0, (posx ip, posy ip, dir ip)) `prepend` update list (path ip) newnode
     prepend newx (x:xs) = (newx:x):xs
     isattributed (Junction _) = True
     isattributed (Lambda _) = True
     isattributed _ = False
     final = fst $ nodes code ([]:temp) trueip
     temp = fst $ nodes code ([]:newlist) falseip
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
 update :: [[PreLexNode]] -- ^The graph to operate on.
    -> RelDirection --  ^Turn taken on last Junctions
    -> Int -- ^ID of new follower to set for the first node in the list.
    -> [[PreLexNode]] -- ^Resulting graph.
 update list@(x:xs) dir following
  | null x && startsattributed xs && dir == Lexer.Left = helpera list following
  | null x && not (null xs) && startsattributed (tail xs) && dir == Lexer.Right = x:head xs:helper (head (tail xs)) following:tail (tail xs)
  | null x = list
  | otherwise = helper x following:xs
   where
    helper ((node, lexeme, _, location):xs) following = (node, lexeme, following, location):xs
    helpera (x:(((node, Junction _, following, location):xs):xss)) attribute = x:(((node, Junction attribute, following, location):xs):xss)
    helpera (x:(((node, Lambda _, following, location):xs):xss)) attribute = x:(((node, Lambda attribute, following, location):xs):xss)
    startsattributed (((_, Junction _, _, _):_):_) = True
    startsattributed (((_, Lambda _, _, _):_):_) = True
    startsattributed _ = False

 -- merges splitted graphs (e.g. Junction)
 -- x3 is the graph until the special node appeared
 -- x2 is the graph that will result in the special attribute
 -- x1 is the graph that will become the follower
 merge :: [[PreLexNode]] -> [[PreLexNode]]
 merge (x1:x2:x3:xs) = (x1 ++ x2 ++ x3):xs

 -- |Move the instruction pointer a single step.
 step :: IDT.Grid2D -- ^Current function in its line representation.
    -> IP -- ^Current instruction pointer.
    -> IP -- ^New instruction pointer.
 step code ip
   | forward `elem` fval = move ip Forward
   | left `elem` lval && right `elem` rval = crash
   | left `elem` lval = move ip Lexer.Left
   | right `elem` rval = move ip Lexer.Right
   | otherwise = crash
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
   | null code = False
   | newy < 0 || newy >= length code = False
   | dir ip `elem` [W, E] && (newx < 0 || newx >= length line) = False
   | otherwise = True
  where
   (newy, newx) = posdir ip reldir
   line = code!!newy

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

 -- |Move the instruction pointer in a relative direction.
 move :: IP -- ^Current instruction pointer.
    -> RelDirection -- ^Relative direction to move in.
    -> IP -- ^New instruction pointer.
 move ip reldir = ip{count = newcount, posx = newx, posy = newy, dir = absolute ip reldir}
  where
   (newy, newx) = posdir ip reldir
   newcount = count ip + 1

 -- |Get the 'Char' at the current position of the instruction pointer.
 current :: IDT.Grid2D -- ^Line representation of the current function.
     -> IP -- ^Current instruction pointer.
     -> Char -- ^'Char' at the current IP position.
 current code ip = charat code (posy ip, posx ip)

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
  | otherwise = (charat code (posdir ip Lexer.Left), charat code (posdir ip Forward), charat code (posdir ip Lexer.Right))

 -- moves both pointers one step
 tuplemove :: (IP, IP) -> (IP, IP)
 tuplemove (ipl, ipr) = (move ipl Forward, move ipr Forward)

 -- saves path information in instruction pointers
 addpath :: (IP, IP) -> (IP, IP)
 addpath (ipl, ipr) = (ipl{path = Lexer.Left}, ipr{path = Lexer.Right})

 -- returns instruction pointers turned for (False, True)
 junctionturns :: IDT.Grid2D -> IP -> (IP, IP)
 junctionturns code ip = tuplecheck $ tuplemove $ addpath $ turning (current code ip) ip
  where
   tuplecheck (ipl, ipr) = (if current code ipl == primary ipl then ipl else crash, if current code ipr == primary ipr then ipr else crash)
   turning char ip
    | char == '<' = case dir ip of
       E -> (ip{dir = NE}, ip{dir = SE})
       SW -> (ip{dir = SE}, ip{dir = W})
       NW -> (ip{dir = W}, ip{dir = NE})
       _ -> (crash, crash)
    | char == '>' = case dir ip of
       W -> (ip{dir = SW}, ip{dir = NW})
       SE -> (ip{dir = E}, ip{dir = SW})
       NE -> (ip{dir = NW}, ip{dir = E})
       _ -> (crash, crash)
    | char == '^' = case dir ip of
       S -> (ip{dir = SE}, ip{dir = SW})
       NE -> (ip{dir = N}, ip{dir = SE})
       NW -> (ip{dir = SW}, ip{dir = N})
       _ -> (crash, crash)
    | char == 'v' = case dir ip of
       N -> (ip{dir = NW}, ip{dir = NE})
       SE -> (ip{dir = NE}, ip{dir = S})
       SW -> (ip{dir = S}, ip{dir = NW})
       _ -> (crash, crash)
    | otherwise = (ip, ip)

 -- returns insturction pointers turned for (Lambda, Reflected)
 lambdadirs :: IP -> (IP, IP)
 lambdadirs ip = addpath (ip, turnaround ip)

 -- make a 180° turn on instruction pointer
 turnaround :: IP -> IP
 turnaround ip = ip{dir = absolute ip{dir = absolute ip{dir = absolute ip{dir = absolute ip Lexer.Left} Lexer.Left} Lexer.Left} Lexer.Left}

 -- |Returns 'Char' at given position, @\' \'@ if position is invalid.
 charat :: IDT.Grid2D -- ^Line representation of current function.
    -> (Int, Int) -- ^Position as (x, y) coordinate.
    -> Char -- ^'Char' at given position.
 charat code _ | null code = ' '
 charat code (y, _) | y < 0 || y >= length code = ' '
 charat code (y, x)
   | x < 0 || x >= length line = ' '
   | otherwise = line!!x
  where
   line = code!!y

 -- |Get the position of a specific heading.
 posdir :: IP -- ^Current instruction pointer.
    -> RelDirection -- ^Current relative direction.
    -> (Int, Int) -- ^New position that results from the given relative movement.
 posdir ip reldir = posabsdir ip (absolute ip reldir)

 -- |Get the position of an absolute direction.
 posabsdir :: IP -- ^Current instruction pointer.
    -> Direction -- ^Current absolute direction.
    -> (Int, Int) -- ^New position that results from the given absolute movement.
 posabsdir ip N = (posy ip - 1, posx ip)
 posabsdir ip NE = (posy ip - 1, posx ip + 1)
 posabsdir ip E = (posy ip, posx ip + 1)
 posabsdir ip SE = (posy ip + 1, posx ip + 1)
 posabsdir ip S = (posy ip + 1, posx ip)
 posabsdir ip SW = (posy ip + 1, posx ip - 1)
 posabsdir ip W = (posy ip, posx ip - 1)
 posabsdir ip NW = (posy ip - 1, posx ip - 1)

 -- |Convert a relative direction into a relative one.
 absolute :: IP -- ^Current instruction pointer.
    -> RelDirection -- ^Relative direction to convert.
    -> Direction -- ^Equivalent absolute direction.
 absolute x Forward = dir x
 absolute (IP {dir=N}) Lexer.Left = NW
 absolute (IP {dir=N}) Lexer.Right = NE
 absolute (IP {dir=NE}) Lexer.Left = N
 absolute (IP {dir=NE}) Lexer.Right = E
 absolute (IP {dir=E}) Lexer.Left = NE
 absolute (IP {dir=E}) Lexer.Right = SE
 absolute (IP {dir=SE}) Lexer.Left = E
 absolute (IP {dir=SE}) Lexer.Right = S
 absolute (IP {dir=S}) Lexer.Left = SE
 absolute (IP {dir=S}) Lexer.Right = SW
 absolute (IP {dir=SW}) Lexer.Left = S
 absolute (IP {dir=SW}) Lexer.Right = W
 absolute (IP {dir=W}) Lexer.Left = SW
 absolute (IP {dir=W}) Lexer.Right = NW
 absolute (IP {dir=NW}) Lexer.Left = W
 absolute (IP {dir=NW}) Lexer.Right = N

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
     | current code ip `elem` "+x*" && next code ip `elem` "v^<>" = (Nothing, crash)
     | forward == ' ' && (left == current code ip || right == current code ip) = (Nothing, crash)
     | forward == ' ' && (left `elem` "v^<>+x*" || right `elem` "v^<>+x*") = (Nothing, crash)
     | otherwise = (Nothing, ip)
    where
     (left, forward, right) = adjacent code ip
   junctioncheck (lexeme, ip)
    | next code ip `elem` "v^<>" = (lexeme, crash)
    | otherwise = (lexeme, ip)
   turn '@' ip = turnaround ip
   turn '|' ip
    | dir ip `elem` [NW, N, NE] = ip{dir = N}
    | dir ip `elem` [SW, S, SE] = ip{dir = S}
   turn '/' ip
    | dir ip `elem` [N, NE, E] = ip{dir = NE}
    | dir ip `elem` [S, SW, W] = ip{dir = SW}
   turn '-' ip
    | dir ip `elem` [NE, E, SE] = ip{dir = E}
    | dir ip `elem` [SW, S, NW] = ip{dir = W}
   turn '\\' ip
    | dir ip `elem` [W, NW, N] = ip{dir = NW}
    | dir ip `elem` [E, SE, S] = ip{dir = SE}
   turn _ ip = ip
   tempip = move ip Forward
   checkstring string = if '!' `elem` string then error EH.strInvalidVarName else string
   pushpop string
    | length string < 2 = Just (Push $ checkstring string)
    | head string == '!' && last string == '!' = Just (Pop (checkstring $ tail $ init string))
		| otherwise = Just (Push $ checkstring string)

 -- |Get ID of the node that has been already visited using the current IP
 -- (direction and coordinates).
 visited :: [[PreLexNode]] -- ^List of nodes to check.
    -> IP -- ^Instruction pointer to use.
    -> Int -- ^ID of visited node or 0 if none.
 visited [] _ = 0
 visited (x:xs) ip = let res = helper x ip in if res > 0 then res else visited xs ip
  where 
   helper [] _ = 0
   helper ((id, _, _, (x, y, d)):xs) ip
    | x == posx ip && y == posy ip && d == dir ip = id
    | otherwise = helper xs ip

 -- |Convert a list of 'PreLexNode's into a list of 'IDT.LexNode's.
 finalize :: [PreLexNode] -- ^'PreLexNode's to convert.
    -> [IDT.LexNode] -- ^Accumulator. Initialize with @[]@.
    -> [IDT.LexNode] -- ^Resulting list of 'IDT.PreLexNode's.
 finalize [] result = result
 finalize ((node, lexeme, following, _):xs) result = finalize xs ((node, lexeme, following):result)

 -- |Initial value for the instruction pointer at the start of a function.
 start :: IP
 start = IP 0 0 0 SE Forward

 -- |An instruction pointer representing a "crash" (fatal error).
 crash :: IP
 crash = IP 0 (-1) (-1) NW Forward

 -- what is the primary rail for the given direction?
 -- mainly used to check if junctions turn away correctly
 primary :: IP -> Char
 primary ip
  | dir ip `elem` [N, S] = '|'
  | dir ip `elem` [E, W] = '-'
  | dir ip `elem` [NE, SW] = '/'
  | dir ip `elem` [NW, SE] = '\\'

 -- |Return valid chars for movement depending on the current direction.
 valids :: IDT.Grid2D -- ^Line representation of current function.
    -> IP -- ^Current instruction pointer.
    -> (String, String, String) -- ^Tuple consisting of:
                                --
                                --     * Valid characters for movement to the (relative) left.
                                --     * Valid characters for movement in the (relative) forward direction.
                                --     * Valid characters for movement to the (relative) right.
 valids code ip = tripleinvert (commandchars ++ dirinvalid ip ++ finvalid ip{dir = absolute ip Lexer.Left}, finvalid ip, commandchars ++ dirinvalid ip ++ finvalid ip{dir = absolute ip Lexer.Right})
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
     (lex, ip) = parse [other] $ IP 0 1 0 E Forward
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

-- vim:ts=2 sw=2 et
