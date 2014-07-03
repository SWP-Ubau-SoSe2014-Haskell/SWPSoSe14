{- |
Module      : Instruction Pointer
Description : Helper module for Lexer to actually move on code.
Maintainer  : Christian H. et al.
License     : MIT
-}
module InstructionPointer (
                           -- constructors
                           start, crash, crashfrom, ipmerge, IP(IP),
                           RelDirection(Left, Right, Forward),
                           Direction (N, NE, E, SE, S, SW, W, NW),
                           -- movement
                           junctionturns, lambdadirs, move, turnaround, 
                           -- attribute/pseudoattribute functions
                           dir, nodecount, posx, posy, count, path, visited,
                           -- other
                           posdir, absolute, nodeadd, turn, current, charat
                          )
 where

 import InterfaceDT as IDT
 import Data.Maybe
 import qualified Data.Map as Map

 -- |An absolute direction.
 data Direction = N | NE | E | SE | S | SW | W | NW deriving (Ord, Eq, Show)
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
			path :: RelDirection,
      -- |Map of Position and Direction to Id
      known :: Map.Map (Int, Int, Direction) Int
    }
  deriving (Show)

 instance Eq IP
  where
   (==) ipl ipr = posx ipl == posx ipr && posy ipl == posy ipr && dir ipl == dir ipr

 -- |Initial value for the instruction pointer at the start of a function.
 start :: IP
 start = IP 0 0 0 SE Forward (Map.singleton (1, 1, SE) 1)

 -- |An instruction pointer representing a "crash" (fatal error).
 crash :: IP
 crash = IP 0 (-1) (-1) NW Forward Map.empty

 crashfrom :: IP -> IP
 crashfrom = ipmerge crash

 ipmerge :: IP -> IP -> IP
 ipmerge ipl ipr = ipl{known = known ipr}

 turn :: Char -> IP -> IP
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

 -- |Returns 'Char' at given position, @\' \'@ if position is invalid.
 charat :: IDT.Grid2D -- ^Line representation of current function.
    -> (Int, Int) -- ^Position as (x, y) coordinate.
    -> Char -- ^'Char' at given position.
 charat code _ | Map.size code == 0 = ' '
 charat code (y, _) | y < 0 || y >= Map.size code = ' '
 charat code (y, x)
   | x < 0 || x >= Map.size line = ' '
   | otherwise = fromJust (Map.lookup x line)
  where
   line = fromJust (Map.lookup y code)

 -- returns instruction pointers turned for (False, True)
 junctionturns :: IDT.Grid2D -> IP -> (IP, IP)
 junctionturns code ip = tuplecheck $ tuplemove $ addpath $ turning (current code ip) ip
  where
   check ip = if current code ip == primary ip then ip else crashfrom ip
   tuplecheck (ipl, ipr) = (check ipl, check ipr)
   turning char ip
    | char == '<' = case dir ip of
       E -> (ip{dir = NE}, ip{dir = SE})
       SW -> (ip{dir = SE}, ip{dir = W})
       NW -> (ip{dir = W}, ip{dir = NE})
       _ -> (crashfrom ip, crashfrom ip)
    | char == '>' = case dir ip of
       W -> (ip{dir = SW}, ip{dir = NW})
       SE -> (ip{dir = E}, ip{dir = SW})
       NE -> (ip{dir = NW}, ip{dir = E})
       _ -> (crashfrom ip, crashfrom ip)
    | char == '^' = case dir ip of
       S -> (ip{dir = SE}, ip{dir = SW})
       NE -> (ip{dir = N}, ip{dir = SE})
       NW -> (ip{dir = SW}, ip{dir = N})
       _ -> (crashfrom ip, crashfrom ip)
    | char == 'v' = case dir ip of
       N -> (ip{dir = NW}, ip{dir = NE})
       SE -> (ip{dir = NE}, ip{dir = S})
       SW -> (ip{dir = S}, ip{dir = NW})
       _ -> (crashfrom ip, crashfrom ip)
    | otherwise = (ip, ip)

 -- returns insturction pointers turned for (Lambda, Reflected)
 lambdadirs :: IP -> (IP, IP)
 lambdadirs ip = addpath (ip, turnaround ip)

 -- moves both pointers one step
 tuplemove :: (IP, IP) -> (IP, IP)
 tuplemove (ipl, ipr) = (move ipl Forward, move ipr Forward)

 nodecount :: IP -> Int
 nodecount ip = Map.size (known ip)

 nodeadd :: IP -> Int -> IP
 nodeadd ip newnode = ip{count = 0, known = Map.insert (posx ip, posy ip, dir ip) newnode (known ip)}

 -- saves path information in instruction pointers
 addpath :: (IP, IP) -> (IP, IP)
 addpath (ipl, ipr) = (ipl{path = InstructionPointer.Left}, ipr{path = InstructionPointer.Right})

 -- make a 180° turn on instruction pointer
 turnaround :: IP -> IP
 turnaround ip = ip{dir = absolute ip{dir = absolute ip{dir = absolute ip{dir = absolute ip InstructionPointer.Left} InstructionPointer.Left} InstructionPointer.Left} InstructionPointer.Left}

 -- |Get ID of the node that has been already visited using the current IP
 -- (direction and coordinates).
 visited :: IP -- ^Instruction pointer to use.
    -> Int -- ^ID of visited node or 0 if none.
 visited ip = Map.findWithDefault 0 (posx ip, posy ip, dir ip) (known ip)

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
 absolute (IP {dir=N}) InstructionPointer.Left = NW
 absolute (IP {dir=N}) InstructionPointer.Right = NE
 absolute (IP {dir=NE}) InstructionPointer.Left = N
 absolute (IP {dir=NE}) InstructionPointer.Right = E
 absolute (IP {dir=E}) InstructionPointer.Left = NE
 absolute (IP {dir=E}) InstructionPointer.Right = SE
 absolute (IP {dir=SE}) InstructionPointer.Left = E
 absolute (IP {dir=SE}) InstructionPointer.Right = S
 absolute (IP {dir=S}) InstructionPointer.Left = SE
 absolute (IP {dir=S}) InstructionPointer.Right = SW
 absolute (IP {dir=SW}) InstructionPointer.Left = S
 absolute (IP {dir=SW}) InstructionPointer.Right = W
 absolute (IP {dir=W}) InstructionPointer.Left = SW
 absolute (IP {dir=W}) InstructionPointer.Right = NW
 absolute (IP {dir=NW}) InstructionPointer.Left = W
 absolute (IP {dir=NW}) InstructionPointer.Right = N

 -- what is the primary rail for the given direction?
 -- mainly used to check if junctions turn away correctly
 primary :: IP -> Char
 primary ip
  | dir ip `elem` [N, S] = '|'
  | dir ip `elem` [E, W] = '-'
  | dir ip `elem` [NE, SW] = '/'
  | dir ip `elem` [NW, SE] = '\\'

-- vim:ts=2 sw=2 et
