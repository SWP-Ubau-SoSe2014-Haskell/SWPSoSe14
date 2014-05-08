module InterfaceDT where

  import qualified LLVM.General.AST as LAST

  -- type definitions --
  type Grid2D  = [String]

  -- |(NodeID (start: 1), Lexeme of Node, NodeID of following Node)
  type LexNode = (Int, Lexeme, Int)
  -- |(FunctionID, Graph of Function as adjacency list)
  type Graph   = (String, [LexNode])
  -- |(FunctionID, [(PathID (start: 1), List of Lexemes to be executed sequentially, PathID of following Path)])
  type AST     = (String, [(Int, [Lexeme], Int)])
  -- |* following Nodes or Pathes could have ID==0, in this case there is no Follower

  -- |Junction Int <=> if false goto Int; if true <=> following node
  data Lexeme = Boom | EOF | Input | Output | Underflow | RType |
    Constant String | Push String | Pop String | Call String | Add | Divide |
    Multiply | Remainder | Substract | Cut | Append | Size | Nil | Cons |
    Breakup | Greater | Equal | Start | Finish | Junction Int deriving (Eq, Show)

  -- interface datatypes --
  data Input2PreProc     = IIP String   deriving (Eq, Show)
  data PreProc2Lexer     = IPL [Grid2D] deriving (Eq, Show)
  data Lexer2SynAna      = ILS [Graph]  deriving (Eq, Show)
  data SynAna2SemAna     = ISS [AST]    deriving (Eq, Show)
  data SemAna2InterCode  = ISI [AST]    deriving (Eq, Show)
  data InterCode2CodeOpt = IIC LAST.Module deriving (Eq, Show)
  data CodeOpt2Backend   = ICB LAST.Module deriving (Eq, Show)
  data Backend2Output    = IBO String   deriving (Eq, Show)
