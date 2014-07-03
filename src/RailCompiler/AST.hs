{- |
Module      : Abstract Syntax Tree
Description : Helper module for Lexer which cares about everything related to the interchageable AST.
Maintainer  : Christian H. et al.
License     : MIT
-}
module AST (fromAST, toAST, parse, adjacent, valids)
 where

 import InterfaceDT as IDT
 import ErrorHandling as EH
 import Data.Maybe
 import Data.List
 import Text.Printf
 import qualified Data.Map as Map
 import InstructionPointer

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
        | curchar == '\\' && escsym == '\\' = ('\\', skip code ip 2)
        | esctrail /= '\\'  = error EH.strNonSymmetricEscape
        | otherwise         = case escsym of
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
-- vim:ts=2 sw=2 et
