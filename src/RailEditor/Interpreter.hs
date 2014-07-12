module Interpreter (
                    addBreak, removeBreak, toggleBreak,
                    interpret, step, reset
                   )
  where
  import qualified InterfaceDT as IDT
  import qualified TextAreaContent as TAC
  import qualified Lexer
  import qualified Data.Map as Map
  import Control.Monad
  import Data.IORef
  import Data.Maybe

  type Funcmap = Map.Map String IDT.PositionedGrid

  addBreak :: TAC.TextAreaContent -> TAC.Position -> IO ()
  addBreak tac position = do
    cnt <- readIORef (TAC.context tac)
    writeIORef (TAC.context tac) cnt{TAC.breakMap = Map.insert position True (TAC.breakMap cnt)}

  removeBreak :: TAC.TextAreaContent -> TAC.Position -> IO ()
  removeBreak tac position = do
    cnt <- readIORef (TAC.context tac)
    writeIORef (TAC.context tac) cnt{TAC.breakMap = Map.delete position (TAC.breakMap cnt)}

  toggleBreak :: TAC.TextAreaContent -> TAC.Position -> IO ()
  toggleBreak tac position = do
    cnt <- readIORef (TAC.context tac)
    if isNothing $ Map.lookup position (TAC.breakMap cnt)
    then addBreak tac position
    else removeBreak tac position

  reset :: TAC.TextAreaContent -> IO ()
  reset tac = writeIORef (TAC.context tac) (TAC.IC [] [] Map.empty)

  init :: TAC.TextAreaContent -> IO ()
  init tac = do
    reset tac
    cnt <- readIORef (TAC.context tac)
    writeIORef (TAC.context tac) cnt{TAC.funcStack = [("main", Lexer.start, Map.empty)]}

  showError :: String -> IO ()
  showError _ = return ()

  showMessage :: String -> IO ()
  showMessage _ = return ()

  abortExecution :: TAC.TextAreaContent -> IO ()
  abortExecution _ = return ()

  interpret :: TAC.TextAreaContent -> IO ()
  interpret tac = do
    funcmap <- getFunctions tac
    dostep tac funcmap
    stop <- needsHalt tac funcmap
    if stop then interpret tac else return ()

  step tac = do
    funcmap <- getFunctions tac
    dostep tac funcmap

  dostep :: TAC.TextAreaContent -> Funcmap -> IO ()
  dostep tac funcmap = do
    cnt <- readIORef (TAC.context tac)
    let fstack = TAC.funcStack cnt
    when (null fstack) (Interpreter.init tac)
    cnt <- readIORef (TAC.context tac)
    let ((fname, ip, vars):xs) = TAC.funcStack cnt
        grid = fst $ fromJust $ Map.lookup fname funcmap
        tmpip = Lexer.step grid ip
        (maylex, newip) = Lexer.parse grid tmpip
    writeIORef (TAC.context tac) cnt{TAC.funcStack = (fname, newip, vars):xs}
    when (isJust maylex) $ perform tac funcmap (fromJust maylex)

  perform :: TAC.TextAreaContent -> Funcmap -> IDT.Lexeme -> IO ()
  perform tac _ IDT.Boom = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt)
    then showError "Empty stack"
    else
      if not $ isString $ head $ TAC.dataStack cnt
      then showError "Element on top of stack is no String"
      else do
        showMessage $ show $ head $ TAC.dataStack cnt
        abortExecution tac
  -- TODO
  perform tac _ IDT.EOF = return ()
  perform tac _ IDT.Input = return ()
  perform tac _ IDT.Output = return ()
  perform tac _ IDT.Underflow = do
    cnt <- readIORef (TAC.context tac)
    writeIORef (TAC.context tac) cnt{TAC.dataStack = (TAC.RailString $ show $ length $ TAC.dataStack cnt):(TAC.dataStack cnt)}
  perform tac _ IDT.RType = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt)
    then showError "Empty stack"
    else writeIORef (TAC.context tac) cnt{TAC.dataStack = (typeOf $ head $ TAC.dataStack cnt):(tail $ TAC.dataStack cnt)}
  perform tac _ (IDT.Constant string) = do
    cnt <- readIORef (TAC.context tac)
    writeIORef (TAC.context tac) cnt{TAC.dataStack = (TAC.RailString string):(TAC.dataStack cnt)}
  perform tac _ (IDT.Push string) = do
    cnt <- readIORef (TAC.context tac)
    let res = searchVar string (TAC.funcStack cnt)
    if isNothing res
    then showError ("'" ++ string ++ "' not found")
    else writeIORef (TAC.context tac) cnt{TAC.dataStack = (fromJust res):(TAC.dataStack cnt)}
  perform tac _ (IDT.Pop string) = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt)
    then showError "Empty stack"
    else do
      let ((fname, ip, vars):xs) = TAC.funcStack cnt
          nvars = Map.insert string (head $ TAC.dataStack cnt) vars
      writeIORef (TAC.context tac) cnt{TAC.dataStack = (tail $ TAC.dataStack cnt), TAC.funcStack = (fname, ip, nvars):xs}
  perform tac _ (IDT.Call string) = do
    cnt <- readIORef (TAC.context tac)
    writeIORef (TAC.context tac) cnt{TAC.funcStack = (string, Lexer.start, Map.empty):(TAC.funcStack cnt)}
  perform tac _ IDT.Add1 = performMath tac (+)
  perform tac _ IDT.Divide = performMath tac (div) -- may be needed to adjust according to compiler
  perform tac _ IDT.Multiply = performMath tac (*)
  perform tac _ IDT.Remainder = performMath tac (rem)
  perform tac _ IDT.Subtract = performMath tac (-)
  perform tac _ IDT.Cut = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt) || null (tail $ TAC.dataStack cnt)
    then showError "Not enough elements on stack"
    else do
      let (e1:e2:xs) = TAC.dataStack cnt
      if not (isNumeric e1) || not (isString e2)
      then showError "Wrong types on stack, number and string expected"
      else do
        let (TAC.RailString s1, TAC.RailString s2) = (e1, e2)
        if length s2 > (read s1 :: Int)
        then showError "Cut number bigger than string length"
        else do
          let (lres, rres) = splitAt (read s1 :: Int) s2
          writeIORef (TAC.context tac) cnt{TAC.dataStack = (TAC.RailString rres):(TAC.RailString lres):xs}
  perform tac _ IDT.Append = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt) || null (tail $ TAC.dataStack cnt)
    then showError "Not enough elements on stack"
    else do
      let (e1:e2:xs) = TAC.dataStack cnt
      if not (isString e1) || not (isString e2)
      then showError "Wrong types on stack, strings expected"
      else do
        let (TAC.RailString s1, TAC.RailString s2) = (e1, e2)
            res = TAC.RailString (s2 ++ s1)
        writeIORef (TAC.context tac) cnt{TAC.dataStack = res:xs}
  perform tac _ IDT.Size = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt)
    then showError "Empty stack"
    else do
      if not $ isString $ head $ TAC.dataStack cnt
      then showError "Wrong type on stack, string expected"
      else do
        let ((TAC.RailString str):xs) = TAC.dataStack cnt
            res = TAC.RailString $ show $ length str
        writeIORef (TAC.context tac) cnt{TAC.dataStack = res:xs}
  perform tac _ IDT.Nil = do
    cnt <- readIORef (TAC.context tac)
    writeIORef (TAC.context tac) cnt{TAC.dataStack = (TAC.RailList []):(TAC.dataStack cnt)}
  perform tac _ IDT.Cons = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt) || null (tail $ TAC.dataStack cnt)
    then showError "Not enough elements on stack"
    else do
      if not $ isList $ head $ tail $ TAC.dataStack cnt
      then showError "Wrong type on second position of stack, list expected"
      else do
        let (x:(TAC.RailList lst):xs) = TAC.dataStack cnt
        writeIORef (TAC.context tac) cnt{TAC.dataStack = (TAC.RailList (x:lst)):xs}
  perform tac _ IDT.Breakup = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt)
    then showError "Empty stack"
    else do
      if not $ isList $ head $ TAC.dataStack cnt
      then showError "Wrong type on stack, list expected"
      else do
        let ((TAC.RailList lst):xs) = TAC.dataStack cnt
        if null lst
        then showError "Empty list cannot be splitted"
        else do
          let (y:ys) = lst
          writeIORef (TAC.context tac) cnt{TAC.dataStack = y:(TAC.RailList ys):xs}
  perform tac _ IDT.Greater = performMath tac (\x y -> if x > y then 1 else 0)
  perform tac _ IDT.Equal = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt) || null (tail $ TAC.dataStack cnt)
    then showError "Not enough elements on stack"
    else do
      let (e1:e2:xs) = TAC.dataStack cnt
          res = if e1 == e2 then TAC.RailString "1" else TAC.RailString "2"
      writeIORef (TAC.context tac) cnt{TAC.dataStack = res:xs}
  perform tac _ IDT.Finish = do
    cnt <- readIORef (TAC.context tac)
    writeIORef (TAC.context tac) cnt{TAC.funcStack = tail $ TAC.funcStack cnt}
  perform tac fmap (IDT.Junction _) = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt)
    then showError "Empty stack"
    else
      if not $ isBool $ head $ TAC.dataStack cnt
      then showError "Wrong type on stack, boolean expected"
      else do
        let ((fname, ip, vars):xs) = TAC.funcStack cnt
            (fip, tip) = Lexer.junctionturns (fst $ fromJust $ Map.lookup fname fmap) ip
            nip = if head (TAC.dataStack cnt) == TAC.RailString "0" then fip else tip
        writeIORef (TAC.context tac) cnt{TAC.dataStack = (tail $ TAC.dataStack cnt), TAC.funcStack = (fname, nip, vars):xs}
  perform tac _ (IDT.Lambda _) = do
    cnt <- readIORef (TAC.context tac)
    let ((fname, ip, vars):xs) = TAC.funcStack cnt
        (lip, nip) = Lexer.lambdadirs ip
    writeIORef (TAC.context tac) cnt{TAC.dataStack = (TAC.RailLambda fname lip):(TAC.dataStack cnt), TAC.funcStack = (fname, nip, vars):xs}

  performMath :: TAC.TextAreaContent -> (Int -> Int -> Int) -> IO ()
  performMath tac op = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt) || null (tail $ TAC.dataStack cnt)
    then showError "Not enough elements on stack"
    else do
      let (e1:e2:xs) = TAC.dataStack cnt
      if not (isNumeric e1) || not (isNumeric e2)
      then showError "Wrong types on stack, numbers expected"
      else do
        let (TAC.RailString n1, TAC.RailString n2) = (e2, e1) -- switching might be necessary
            res = TAC.RailString $ show $ (read n1 :: Int) `op` (read n2 :: Int)
        writeIORef (TAC.context tac) cnt{TAC.dataStack = res:xs}

  searchVar :: String -> [(String, Lexer.IP, Map.Map String TAC.RailType)] -> Maybe TAC.RailType
  searchVar _ [] = Nothing
  searchVar var ((_, _, vars):xs)
    | isNothing $ Map.lookup var vars = searchVar var xs
    | otherwise = Map.lookup var vars

  getFunctions :: TAC.TextAreaContent -> IO Funcmap
  getFunctions tac = do
    pGrid <- TAC.getPositionedGrid tac
    let (IDT.IPL funcs) = pGrid
    resmap <- return Map.empty
    addnames resmap funcs
   where
    addnames resmap [] = return resmap
    addnames resmap (x:xs) = do
      fname <- funcname (fst x)
      newmap <- return $ Map.insert fname x resmap
      addnames newmap xs

  getFunctionWith :: TAC.TextAreaContent -> TAC.Position -> IO (String, IDT.Grid2D)
  getFunctionWith tac position = do
    pGrid <- TAC.getPositionedGrid tac
    let (IDT.IPL funcs) = pGrid
        funclist = filter isJust $ map (\(f, y) -> if y <= (snd position) then Just f else Nothing) funcs
    if null funclist
    then return ("", Map.empty)
    else do
      let res = fromJust $ last funclist
      fname <- funcname res
      return $ (fname, res)

  funcname :: IDT.Grid2D -> IO String
  funcname code = case Lexer.funcname code of
    (Left fname) -> return fname
    (Right error) -> do
      showError "No function name"
      return ""

  needsHalt :: TAC.TextAreaContent -> Funcmap -> IO (Bool)
  needsHalt tac fmap = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.funcStack cnt)
    then return True
    else do
      let (fname, ip, _) = head $ TAC.funcStack cnt
      pos <- return (Lexer.posx ip, Lexer.posy ip + (snd $ fromJust $ Map.lookup fname fmap))
      return $ Map.findWithDefault False pos (TAC.breakMap cnt)

  isNumeric :: TAC.RailType -> Bool
  isNumeric (TAC.RailString string) = and $ map (\c -> c `elem` "0123456789") string
  isNumeric _ = False

  isList :: TAC.RailType -> Bool
  isList (TAC.RailList _) = True
  isList _ = False

  isNil :: TAC.RailType -> Bool
  isNil (TAC.RailList []) = True
  isNil _ = False

  isString :: TAC.RailType -> Bool
  isString (TAC.RailString _) = True
  isString _ = False

  isBool :: TAC.RailType -> Bool
  isBool (TAC.RailString string) = string == "0" || string == "1"
  isBool _ = False

  isLambda :: TAC.RailType -> Bool
  isLambda (TAC.RailLambda _ _) = True
  isLambda _ = False

  typeOf :: TAC.RailType -> TAC.RailType
  typeOf var
    | isString var = TAC.RailString "string"
    | isNil var = TAC.RailString "nil"
    | isList var = TAC.RailString "list"
    | isLambda var = TAC.RailString "lambda"
