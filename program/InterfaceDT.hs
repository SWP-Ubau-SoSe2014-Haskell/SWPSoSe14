module InterfaceDT
 where
  
  -- type definitions --
  type Grid2D  = [String]
  type LexNode = (Int, Lexeme, Int)
  type Graph   = (String, [LexNode])
  type AST     = (String, [(Int, [Lexeme], Int)])
  
  data Lexeme = Boom | EOF | Input | Output | Underflow | RType | Constant String | Push String | Pop String | Call String | Add | Divide | Multiply | Remainder | Substract | Cut | Append | Size | Nil | Cons | Breakup | Greater | Equal | Start | Finish | Junction Int deriving (Eq, Show)
  
  
  -- interface datatypes --
  data Input2PreProc     = IIP String   deriving (Eq, Show)
  data PreProc2Lexer     = IPL [Grid2D] deriving (Eq, Show)
  data Lexer2SynAna      = ILS [Graph]  deriving (Eq, Show)
  data SynAna2SemAna     = ISS [AST]    deriving (Eq, Show)
  data SemAna2InterCode  = ISI String   deriving (Eq, Show)  -- todo!
  data InterCode2CodeOpt = IIC String   deriving (Eq, Show)  -- todo!
  data CodeOpt2Backend   = ICB String   deriving (Eq, Show)  -- todo!
  data Backend2Output    = IBO String   deriving (Eq, Show)  -- todo!
   
