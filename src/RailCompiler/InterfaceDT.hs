{- |
Module      : InterfaceDT.hs
Description : .
Maintainer  : Nicolas Lehmann
License     : MIT

Defining algebraic data types for all compiler stages. Each module in the
pipeline has two corresponding algebraic data types, one defining the input and
the other defining the output. The output data type of a compiler stage is the
input data type of the following compiler stage. The algebraic data types ensure
a clean interface between the modules.

-}
module InterfaceDT where

  import qualified LLVM.General.AST as LAST

  -- type definitions --
  type Grid2D  = [String]
  -- Int gives the line on which the function starts
  type PositionedGrid = (Grid2D, Int)
  -- |(NodeID (start: 1), Lexeme of Node, NodeID of following Node (0 if none))
  type LexNode = (Int, Lexeme, Int)
  -- |(FunctionID, Graph of Function as adjacency list)
  type Graph   = (String, [LexNode])
  -- |(FunctionID, [(PathID (start: 1), List of Lexemes to be executed sequentially, PathID of following Path)])
  type AST     = (String, [(Int, [Lexeme], Int)])
  -- |* following Nodes or Pathes could have ID==0, in this case there is no Follower

  -- |Junction Int <=> if false goto Int; if true <=> following node
	-- |Lambda Int <=> Int holds node of anonymous function
  data Lexeme = NOP | Boom | EOF | Input | Output | Underflow | RType |
    Constant String | Push String | Pop String | Call String | Add1 | Divide |
    Multiply | Remainder | Subtract | Cut | Append | Size | Nil | Cons |
    Breakup | Greater | Equal | Start | Finish | Junction Int | Lambda Int deriving (Eq, Show)

  -- interface datatypes --
  data Input2PreProc     = IIP String   deriving (Eq, Show)
  data PreProc2Lexer     = IPL [PositionedGrid] deriving (Eq, Show)
  data Lexer2SynAna      = ILS [Graph]  deriving (Eq, Show)
  data SynAna2SemAna     = ISS [AST]    deriving (Eq, Show)
  data SemAna2InterCode  = ISI [AST]    deriving (Eq, Show)
  data InterCode2CodeOpt = IIC LAST.Module deriving (Eq, Show)
  data CodeOpt2Backend   = ICB LAST.Module deriving (Eq, Show)
  data Backend2Output    = IBO (IO String)
