module InterfaceDT
 where
  
  -- type definitions --
  type Grid2D  = [String]
  type LexNode = (Int, String, Int, Int)
  type Graph   = [LexNode]
  
  -- interface datatypes --
  data Input2PreProc     = IIP String   deriving Show
  data PreProc2Lexer     = IPL [Grid2D] deriving Show
  data Lexer2SynAna      = ILS [Graph]  deriving Show
  data SynAna2SemAna     = ISS String   deriving Show  -- todo!
  data SemAna2InterCode  = ISI String   deriving Show  -- todo!
  data InterCode2CodeOpt = IIC String   deriving Show  -- todo!
  data CodeOpt2Backend   = ICB String   deriving Show  -- todo!
  data Backend2Output    = IBO String   deriving Show  -- todo!
   