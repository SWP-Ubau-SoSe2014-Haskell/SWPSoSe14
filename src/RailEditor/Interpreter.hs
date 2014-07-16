module Interpreter (
                    addBreak, removeBreak, toggleBreak,
                    interpret, step, reset
                   )
  where
  import qualified Graphics.UI.Gtk as Gtk
  import qualified InterfaceDT as IDT
  import qualified TextAreaContent as TAC
  import qualified Lexer
  import qualified Data.Map as Map
  import Control.Monad
  import Data.IORef
  import Data.Maybe
  import Data.List
  import qualified Data.Foldable

  type Funcmap = Map.Map String IDT.PositionedGrid

  getCurrentText buffer = do
    start <- Gtk.textBufferGetStartIter buffer
    end <- Gtk.textBufferGetEndIter buffer
    Gtk.textBufferGetText buffer start end True

  checkStep tac flag action = do
    cnt <- readIORef (TAC.context tac)
    isBlocked <- blocked tac
    let flags = TAC.railFlags cnt
    if flag `elem` flags && null (TAC.funcStack cnt)
    then showError tac "Please reset, before you change the execution mode"
    else do
      unless (TAC.Interpret `elem` flags) $ writeIORef (TAC.context tac) cnt{TAC.railFlags = TAC.Interpret:flags}
      if TAC.Blocked `elem` flags
      then do
        putStrLn "is blocked"
        unless isBlocked $ do
          putStrLn "unblock"
          inputAfterBlock tac
          writeIORef (TAC.context tac) cnt{TAC.railFlags = delete TAC.Blocked flags}
      else action tac

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
  reset tac = do
    abortExecution tac
    Gtk.textBufferSetText (snd $ TAC.buffer tac) ""
    Gtk.textBufferSetText (fst $ TAC.buffer tac) ""

  abortExecution :: TAC.TextAreaContent -> IO ()
  abortExecution tac = do
    cnt <- readIORef (TAC.context tac)
    writeIORef (TAC.context tac) (TAC.IC [] [] (TAC.breakMap cnt) 0 (-1,-1) [])

  init :: TAC.TextAreaContent -> IO ()
  init tac = do
    reset tac
    cnt <- readIORef (TAC.context tac)
    writeIORef (TAC.context tac) cnt{TAC.funcStack = [("main", Lexer.start, Map.empty)]}

  blocked :: TAC.TextAreaContent -> IO Bool
  blocked tac = do
    cnt <- readIORef (TAC.context tac)
    let offset = TAC.inputOffset cnt
        buffer = fst $ TAC.buffer tac
    currentText <- getCurrentText buffer
    putStrLn $ "off:" ++ show offset ++ "; "++ show (length currentText)
    return $ offset  > length currentText

  showError :: TAC.TextAreaContent -> String -> IO ()
  showError tac string = do
    showMessage tac ("Error: " ++ string)
    abortExecution tac

  showMessage :: TAC.TextAreaContent -> String -> IO ()
  showMessage tac message = showRawMessage (message ++ "\n") (snd $ TAC.buffer tac)

  showRawMessage :: String -> Gtk.TextBuffer -> IO ()
  showRawMessage message buffer = do
    end <- Gtk.textBufferGetEndIter buffer
    Gtk.textBufferInsert buffer end message

  updateCurIPPos :: TAC.TextAreaContent -> Funcmap -> IO ()
  updateCurIPPos tac fmap = do
    cnt <- readIORef (TAC.context tac)
    unless (null $ TAC.funcStack cnt) $ do
      let ((fname, ip, _):_) = TAC.funcStack cnt
          offset = snd $ fromJust $ Map.lookup fname fmap
      writeIORef (TAC.context tac) cnt{TAC.curIPPos = (Lexer.posx ip, Lexer.posy ip + offset)}

  interpret :: TAC.TextAreaContent -> IO ()
  interpret tac = checkStep tac TAC.Step $ do
    putStrLn "noBlock"
    funcmap <- getFunctions tac
    dostep tac funcmap
    updateCurIPPos tac funcmap
    stop <- needsHalt tac funcmap
    unless stop $ interpret tac

  step tac = checkStep tac TAC.Interpret $ do
    funcmap <- getFunctions tac
    dostep tac funcmap
    updateCurIPPos tac funcmap

  inputAfterBlock :: TAC.TextAreaContent -> IO ()
  inputAfterBlock tac = do
    cnt <- readIORef (TAC.context tac)
    let offset = TAC.inputOffset cnt
    let buffer = fst $ TAC.buffer tac
    currentText <- getCurrentText buffer
    putStrLn "read"
    let char = currentText !! offset
    writeIORef (TAC.context tac) cnt{TAC.dataStack = TAC.RailString [char]:TAC.dataStack cnt}

  dostep :: TAC.TextAreaContent -> Funcmap -> IO ()
  dostep tac funcmap = do
    cnt <- readIORef (TAC.context tac)
    let fstack = TAC.funcStack cnt
    when (null fstack) (Interpreter.init tac)
    cnt <- readIORef (TAC.context tac)
    let ((fname, ip, vars):xs) = TAC.funcStack cnt
    if isNothing $ Map.lookup fname funcmap
    then showError tac $ "Function '" ++ fname ++ "' not found"
    else do
      let
          grid = fst $ fromJust $ Map.lookup fname funcmap
          tmpip = Lexer.step grid ip
          (maylex, newip) = Lexer.parse grid tmpip
      writeIORef (TAC.context tac) cnt{TAC.funcStack = (fname, newip, vars):xs}
      Data.Foldable.forM_ maylex (perform tac funcmap)

  perform :: TAC.TextAreaContent -> Funcmap -> IDT.Lexeme -> IO ()
  perform tac _ IDT.Boom = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt)
    then showError tac "Empty stack"
    else
      if not $ isString $ head $ TAC.dataStack cnt
      then showError tac "Element on top of stack is no String"
      else do
        showMessage tac (show $ head $ TAC.dataStack cnt)
        abortExecution tac
  -- TODO
  perform tac _ IDT.EOF = return ()
  perform tac _ IDT.Input = do
    cnt <- readIORef (TAC.context tac)
    let offset = TAC.inputOffset cnt
    writeIORef (TAC.context tac) cnt{TAC.inputOffset = succ offset}
    isBlocked <- blocked tac
    if isBlocked
    then do
      let flags = TAC.railFlags cnt
      writeIORef (TAC.context tac) cnt{TAC.railFlags = TAC.Blocked:flags}
      putStrLn "blocken"
      return ()
    else do
      let buffer = fst $ TAC.buffer tac
      start <- Gtk.textBufferGetStartIter buffer
      end <- Gtk.textBufferGetEndIter buffer
      currentText <- Gtk.textBufferGetText buffer start end True
      writeIORef (TAC.context tac) cnt{TAC.dataStack = TAC.RailString [currentText !! offset]:TAC.dataStack cnt}

  perform tac _ IDT.Output = do
    cnt <- readIORef (TAC.context tac)
    let dataSt = TAC.dataStack cnt
    if null dataSt
    then showError tac "Empty Stack"
    else if (\(TAC.RailString _:_) -> True) dataSt
         then do 
            showRawMessage ((\(TAC.RailString t:_) -> t) dataSt) (snd $ TAC.buffer tac)
            writeIORef (TAC.context tac) cnt{TAC.dataStack = tail (TAC.dataStack cnt)}
         else showError tac "Element on top of the stack is no String"
  perform tac _ IDT.Underflow = do
    cnt <- readIORef (TAC.context tac)
    writeIORef (TAC.context tac) cnt{TAC.dataStack = (TAC.RailString $ show $ length $ TAC.dataStack cnt):TAC.dataStack cnt}
  perform tac _ IDT.RType = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt)
    then showError tac "Empty stack"
    else writeIORef (TAC.context tac) cnt{TAC.dataStack = typeOf (head $ TAC.dataStack cnt):tail (TAC.dataStack cnt)}
  perform tac _ (IDT.Constant string) = do
    cnt <- readIORef (TAC.context tac)
    writeIORef (TAC.context tac) cnt{TAC.dataStack = TAC.RailString string:TAC.dataStack cnt}
  perform tac _ (IDT.Push string) = do
    cnt <- readIORef (TAC.context tac)
    let res = searchVar string (TAC.funcStack cnt)
    if isNothing res
    then showError tac ("Variable '" ++ string ++ "' not found")
    else writeIORef (TAC.context tac) cnt{TAC.dataStack = fromJust res:TAC.dataStack cnt}
  perform tac _ (IDT.Pop string) = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt)
    then showError tac "Empty stack"
    else do
      let ((fname, ip, vars):xs) = TAC.funcStack cnt
          nvars = Map.insert string (head $ TAC.dataStack cnt) vars
      writeIORef (TAC.context tac) cnt{TAC.dataStack = tail $ TAC.dataStack cnt, TAC.funcStack = (fname, ip, nvars):xs}
  perform tac _ (IDT.Call string) = do
    cnt <- readIORef (TAC.context tac)
    if null string
    then
      if null (TAC.dataStack cnt)
      then showError tac "Empty stack"
      else
        if not $ isLambda $ head $ TAC.dataStack cnt
        then showError tac "Wrong type on stack, string expected"
        else do
          let (TAC.RailLambda fn ip map:xs) = TAC.dataStack cnt
          writeIORef (TAC.context tac) cnt{TAC.funcStack = (fn, ip, map):TAC.funcStack cnt, TAC.dataStack = xs}
    else writeIORef (TAC.context tac) cnt{TAC.funcStack = (string, Lexer.start, Map.empty):TAC.funcStack cnt}
  perform tac _ IDT.Add1 = performMath tac (+)
  perform tac _ IDT.Divide = performMath tac div -- may be needed to adjust according to compiler
  perform tac _ IDT.Multiply = performMath tac (*)
  perform tac _ IDT.Remainder = performMath tac rem
  perform tac _ IDT.Subtract = performMath tac (-)
  perform tac _ IDT.Cut = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt) || null (tail $ TAC.dataStack cnt)
    then showError tac "Not enough elements on stack"
    else do
      let (e1:e2:xs) = TAC.dataStack cnt
      if not (isNumeric e1) || not (isString e2)
      then showError tac "Wrong types on stack, number and string expected"
      else do
        let (TAC.RailString s1, TAC.RailString s2) = (e1, e2)
        if length s2 > (read s1 :: Int)
        then showError tac "Cut number bigger than string length"
        else do
          let (lres, rres) = splitAt (read s1 :: Int) s2
          writeIORef (TAC.context tac) cnt{TAC.dataStack = TAC.RailString rres:TAC.RailString lres:xs}
  perform tac _ IDT.Append = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt) || null (tail $ TAC.dataStack cnt)
    then showError tac "Not enough elements on stack"
    else do
      let (e1:e2:xs) = TAC.dataStack cnt
      if not (isString e1) || not (isString e2)
      then showError tac "Wrong types on stack, strings expected"
      else do
        let (TAC.RailString s1, TAC.RailString s2) = (e1, e2)
            res = TAC.RailString (s2 ++ s1)
        writeIORef (TAC.context tac) cnt{TAC.dataStack = res:xs}
  perform tac _ IDT.Size = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt)
    then showError tac "Empty stack"
    else
      if not $ isString $ head $ TAC.dataStack cnt
      then showError tac "Wrong type on stack, string expected"
      else do
        let (TAC.RailString str:xs) = TAC.dataStack cnt
            res = TAC.RailString $ show $ length str
        writeIORef (TAC.context tac) cnt{TAC.dataStack = res:xs}
  perform tac _ IDT.Nil = do
    cnt <- readIORef (TAC.context tac)
    writeIORef (TAC.context tac) cnt{TAC.dataStack = TAC.RailList []:TAC.dataStack cnt}
  perform tac _ IDT.Cons = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt) || null (tail $ TAC.dataStack cnt)
    then showError tac "Not enough elements on stack"
    else
      if not $ isList $ head $ tail $ TAC.dataStack cnt
      then showError tac "Wrong type on second position of stack, list expected"
      else do
        let (x:TAC.RailList lst:xs) = TAC.dataStack cnt
        writeIORef (TAC.context tac) cnt{TAC.dataStack = TAC.RailList (x:lst):xs}
  perform tac _ IDT.Breakup = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt)
    then showError tac "Empty stack"
    else
      if not $ isList $ head $ TAC.dataStack cnt
      then showError tac "Wrong type on stack, list expected"
      else do
        let (TAC.RailList lst:xs) = TAC.dataStack cnt
        if null lst
        then showError tac "Empty list cannot be splitted"
        else do
          let (y:ys) = lst
          writeIORef (TAC.context tac) cnt{TAC.dataStack = y:TAC.RailList ys:xs}
  perform tac _ IDT.Greater = performMath tac (\x y -> if x > y then 1 else 0)
  perform tac _ IDT.Equal = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt) || null (tail $ TAC.dataStack cnt)
    then showError tac "Not enough elements on stack"
    else do
      let (e1:e2:xs) = TAC.dataStack cnt
          res = TAC.RailString (if e1 == e2 then "1" else "0")
      writeIORef (TAC.context tac) cnt{TAC.dataStack = res:xs}
  perform tac _ IDT.Finish = do
    cnt <- readIORef (TAC.context tac)
    writeIORef (TAC.context tac) cnt{TAC.funcStack = tail $ TAC.funcStack cnt, TAC.curIPPos = (-1, -1)}
  perform tac fmap (IDT.Junction _) = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt)
    then showError tac "Empty stack"
    else
      if not $ isBool $ head $ TAC.dataStack cnt
      then showError tac "Wrong type on stack, boolean expected"
      else do
        let ((fname, ip, vars):xs) = TAC.funcStack cnt
            (fip, tip) = Lexer.junctionturns (fst $ fromJust $ Map.lookup fname fmap) ip
            nip = if head (TAC.dataStack cnt) == TAC.RailString "0" then fip else tip
        writeIORef (TAC.context tac) cnt{TAC.dataStack = tail $ TAC.dataStack cnt, TAC.funcStack = (fname, nip, vars):xs}
  perform tac _ (IDT.Lambda _) = do
    cnt <- readIORef (TAC.context tac)
    let ((fname, ip, vars):xs) = TAC.funcStack cnt
        (lip, nip) = Lexer.lambdadirs ip
    writeIORef (TAC.context tac) cnt{TAC.dataStack = TAC.RailLambda fname lip vars:TAC.dataStack cnt, TAC.funcStack = (fname, nip, vars):xs}

  performMath :: TAC.TextAreaContent -> (Int -> Int -> Int) -> IO ()
  performMath tac op = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt) || null (tail $ TAC.dataStack cnt)
    then showError tac "Not enough elements on stack"
    else do
      let (e1:e2:xs) = TAC.dataStack cnt
      if not (isNumeric e1) || not (isNumeric e2)
      then showError tac "Wrong types on stack, numbers expected"
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
    let resmap = Map.empty
    addnames resmap funcs
   where
    addnames resmap [] = return resmap
    addnames resmap (x:xs) = do
      fname <- funcname tac (fst x)
      let newmap = Map.insert fname x resmap
      addnames newmap xs

  getFunctionWith :: TAC.TextAreaContent -> TAC.Position -> IO (String, IDT.Grid2D)
  getFunctionWith tac position = do
    pGrid <- TAC.getPositionedGrid tac
    let (IDT.IPL funcs) = pGrid
        funclist = filter isJust $ map (\(f, y) -> if y <= snd position then Just f else Nothing) funcs
    if null funclist
    then return ("", Map.empty)
    else do
      let res = fromJust $ last funclist
      fname <- funcname tac res
      return (fname, res)

  funcname :: TAC.TextAreaContent -> IDT.Grid2D -> IO String
  funcname tac code = case Lexer.funcname code of
    (Left fname) -> return fname
    (Right error) -> do
      showError tac "No function name"
      return ""

  needsHalt :: TAC.TextAreaContent -> Funcmap -> IO Bool
  needsHalt tac fmap = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.funcStack cnt)
    then return True
    else do
      putStrLn "check"
      let (fname, ip, _) = head $ TAC.funcStack cnt
      let pos = (Lexer.posx ip, Lexer.posy ip + snd (fromJust $ Map.lookup fname fmap))
      print (Map.findWithDefault False pos (TAC.breakMap cnt))
      return $ Map.findWithDefault False pos (TAC.breakMap cnt)

  isNumeric :: TAC.RailType -> Bool
  isNumeric (TAC.RailString string) = all (`elem` "0123456789") string
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
  isLambda TAC.RailLambda{} = True
  isLambda _ = False

  typeOf :: TAC.RailType -> TAC.RailType
  typeOf var
    | isString var = TAC.RailString "string"
    | isNil var = TAC.RailString "nil"
    | isList var = TAC.RailString "list"
    | isLambda var = TAC.RailString "lambda"
