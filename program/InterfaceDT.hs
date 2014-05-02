module InterfaceDT
 where
  
  -- type definitions --
  type Grid2D  = [String]
  type LexNode = (Int, String, Int, Int)
  type Graph   = [LexNode]
  
  -- interface datatypes --
  data Input2PreProc     = IIP String   deriving (Eq, Show)
  data PreProc2Lexer     = IPL [Grid2D] deriving (Eq, Show)
  data Lexer2SynAna      = ILS [Graph]  deriving (Eq, Show)
  data SynAna2SemAna     = ISS String   deriving (Eq, Show)  -- todo!
  data SemAna2InterCode  = ISI String   deriving (Eq, Show)  -- todo!
  data InterCode2CodeOpt = IIC String   deriving (Eq, Show)  -- todo!
  data CodeOpt2Backend   = ICB String   deriving (Eq, Show)  -- todo!
  data Backend2Output    = IBO String   deriving (Eq, Show)  -- todo!
   