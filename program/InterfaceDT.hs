module InterfaceDT
 where
  
  -- interface datatypes --
  data Input2PreProc     = IIP String deriving Show  -- todo!
  data PreProc2Lexer     = IPL String deriving Show  -- todo!
  data Lexer2SynAna      = ILS String deriving Show  -- todo!
  data SynAna2SemAna     = ISS String deriving Show  -- todo!
  data SemAna2InterCode  = ISI String deriving Show  -- todo!
  data InterCode2CodeOpt = IIC String deriving Show  -- todo!
  data CodeOpt2Backend   = ICB String deriving Show  -- todo!
  data Backend2Output    = IBO String deriving Show  -- todo!
   