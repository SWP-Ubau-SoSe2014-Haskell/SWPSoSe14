module Lexer (
              process   -- main function of the module "Lexer"
             )
 where

 -- imports --
 import InterfaceDT as IDT

 -- added identifier for nodes to check when we have circles
 type PreLexNode = (Int, IDT.Lexeme, Int, (Int, Int, Direction))
 data Direction = N | NE | E | SE | S | SW | W | NW deriving Eq
 data RelDirection = Left | Forward | Right
 -- instruction pointer consisting of position and an orientation
 data IP = IP { posx :: Int, posy :: Int, dir :: Direction } deriving Eq
 
 -- functions --
 process :: IDT.PreProc2Lexer -> IDT.Lexer2SynAna
 process input = map processfn input

 -- process one function
 processfn :: IDT.Grid2D -> IDT.Graph
 processfn code@(x:xs) = (funcname x, finalize [] $ nodes code [] start)

 -- get the name of the given function
 funcname :: String -> String
 funcname line = takeWhile (/='\'') $ tail $ dropWhile (/='\'') line

 -- get the nodes for the given function
 nodes :: IDT.Grid2D -> [PreLexNode] -> IP -> ([PreLexNode], IP)
 nodes code list ip
  | current code ip == ' ' = list
  | otherwise = nodes code newlist newip
     where -- parse, handle, move
      newip = step code tempip
      (newlist, tempip) = handle code list ip

 -- TODO: changing movemend based on used rails
 handle :: IDT.Grid2D -> [PreLexNode] -> IP -> ([PreLexNode], IP)
 handle code list ip = helper code list ip (parse code ip)
  where
   helper _ list ip Nothing = (list, ip)
   helper code list ip (Just lexeme) = (newlist, ip)
    where
     newnode = (length list) + 1
     newlist = (newnode, lexeme, 0, (posx ip, posy ip, dir ip)):(update list newnode)

 -- changing following node of previous node
 update :: [PreLexNode] -> Int -> [PreLexNode]
 update [] _ = []
 update ((node, lexeme, _, location):xs) following = (node, lexeme, following, location)

 -- move the instruction pointer a singe step
 step :: IDT.Grid2D -> IP -> IP
 step code ip
   | forward `elem` valids = move ip Forward
   | left `elem` valids && right `elem` valids = crash
   | left `elem` valids = move ip Lexer.Left
   | right `elem` valids = move ip Lexer.Right
   | otherwise = crash
  where
   (left, forward, right) = adjacent code ip

 stepwhile :: IDT.Grid2D -> IP -> (Char -> Bool) -> (String, IP)
 stepwhile code ip fn
   | not (fn curchar) = ("", ip)
   | otherwise = (curchar:resstring, resip)
  where
   curchar = current code ip
   (resstring, resip) = stepwhile code (move ip Forward) fn

 move :: IP -> RelDirection -> IP
 move ip reldir = ip{posx = newx, posy = newy, dir = absolute reldir}
  where
   (newx, newy) = posdir ip reldir

 current :: IDT.Grid2D -> IP -> Char
 current code ip = charat code (posy ip, posx ip)

 -- get (left secondary, primary, right secondary) symbols
 adjacent :: IDT.Grid2D -> IP -> (Char, Char, Char)
 adjacent code ip = (charat code (posdir ip Lexer.Left), charat code (posdir ip Forward), charat code (posdir ip Lexer.Right))

 -- returns char at given position, ' ' if position is invalid
 charat :: IDT.Grid2D -> (Int, Int) -> Char
 charat code _ | length code == 0 = ' '
 charat code (y, _) | y < 0 || y >= (length code) = ' '
 charat code (_, x) | x < 0 || x >= (length (head code)) = ' '
 charat code (y, x) = (code!!y)!!x

 -- get the position of a specific heading
 posdir :: IP -> RelDirection -> (Int, Int)
 posdir ip reldir = posabsdir ip (absolute reldir)
 posabsdir :: IP -> Direction -> (Int, Int)
 posabsdir ip N = ((posy ip) - 1, posx ip)
 posabsdir ip NE = ((posy ip) - 1, (posx ip) + 1)
 posabsdir ip E = (posy ip, (posx ip) + 1)
 posabsdir ip SE = ((posy ip) + 1, (posx ip) + 1)
 posabsdir ip S = ((posy ip) + 1, posx ip)
 posabsdir ip SW = ((posy ip) + 1, (posx ip) - 1)
 posabsdir ip W = (posy ip, (posx ip) - 1)
 posabsdir ip NW = ((posy ip) - 1, (posx ip) - 1)

 -- get the absolute direction out of a relative one
 absolute :: IP -> RelDirection -> Direction
 absolute x Forward = x
 absolute N Lexer.Left = NW
 absolute N Lexer.Right = NE
 absolute NE Lexer.Left = N
 absolute NE Lexer.Right = E
 absolute E Lexer.Left = NE
 absolute E Lexer.Right = SE
 absolute SE Lexer.Left = E
 absolute SE Lexer.Right = S
 absolute S Lexer.Left = SE
 absolute S Lexer.Right = SW
 absolute SW Lexer.Left = S
 absolute SW Lexer.Right = W
 absolute W Lexer.Left = SW
 absolute W Lexer.Right = NW
 absolute NW Lexer.Left = W
 absolute NW Lexer.Right = N

 -- get the lexem out of a char
 parse :: IDT.Grid2D -> IP -> (Maybe IDT.Lexeme, ip)
 parse code ip count = case (current code ip) of
   'b' -> (Just Boom, ip)
   'e' -> (Just EOF, ip)
   'i' -> (Just Input, ip)
   'o' -> (Just Output, ip)
   'u' -> (Just Underflow, ip)
   '?' -> (Just RType, ip)
   'a' -> (Just Add, ip)
   'd' -> (Just Divide, ip)
   'm' -> (Just Multiply, ip)
   'r' -> (Just Remainder, ip)
   's' -> (Just Substract, ip)
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
   'v' -> (Just (Junction 0), ip)
   '^' -> (Just (Junction 0), ip)
   '>' -> (Just (Junction 0), ip)
   '<' -> (Just (Junction 0), ip)
   '[' -> let (string, newip) = stepwhile code ip (/= ']') in (Just (Constant string), newip)
   ']' -> let (string, newip) = stepwhile code ip (/= '[') in (Just (Constant string), newip)
   '{' -> let (string, newip) = stepwhile code ip (/= '}') in (Just (Call string), newip)
   '}' -> let (string, newip) = stepwhile code ip (/= '{') in (Just (Call string), newip)
   '(' -> let (string, newip) = stepwhile code ip (/= ')') in (pushpop string, newip)
   ')' -> let (string, newip) = stepwhile code ip (/= '(') in (pushpop string, newip)
   _ -> Nothing
  where
   pushpop string
    | string == "" = Just (Push string)
    | string!!1 == '!' && last string == '!' = Just (Pop (tail $ init string))
		| otherwise = Just (Push string)

 visited :: [PreLexNode] -> IP -> Bool
 visited [] _ = False
 visited ((_, _, _, (x, y, lmode)):xs) ip = (x == (posx ip) && y == (posy ip)) || visited xs ip

 finalize :: [PreLexNode] -> [IDT.LexNode] -> [IDT.LexNode]
 finalize [] result = result
 finalize ((node, lexeme, following, _):xs) result = finalize xs (node, lexeme, following):result

 start :: IP
 start = IP 0 0 SE
 crash = IP (-1) (-1) SE
 valids :: String
 valids = "\\|-/abcdefgimnopqrstuvxz+*<>^@$#&:~0123456789{}[]()?"

-- vim:ts=2 sw=2 et
