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
  perform tac _ IDT.Add1 = return ()
  perform tac _ IDT.Divide = return ()
  perform tac _ IDT.Multiply = return ()
  perform tac _ IDT.Remainder = return ()
  perform tac _ IDT.Subtract = return ()
  --funcStack :: [(String, Lexer.IP, Map.Map String RailType)],
  perform tac _ IDT.Cut = return ()
  perform tac _ IDT.Append = return ()
  perform tac _ IDT.Size = return ()
  perform tac _ IDT.Nil = return ()
  perform tac _ IDT.Cons = return ()
  perform tac _ IDT.Breakup = return ()
  perform tac _ IDT.Greater = do
    cnt <- readIORef (TAC.context tac)
    if null (TAC.dataStack cnt) || null (tail $ TAC.dataStack cnt)
    then showError "Not enough elements on stack"
    else do
      let (e1:e2:xs) = TAC.dataStack cnt
      if not (isNumeric e1) || not (isNumeric e2)
      then showError "Wrong types on stack, numbers expected"
      else do
        let (TAC.RailString n1, TAC.RailString n2) = (e1, e2)
            res = if n1 < n2 then TAC.RailString "1" else TAC.RailString "2"
        writeIORef (TAC.context tac) cnt{TAC.dataStack = res:xs}
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
      then showError "Wrong type on stack"
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
{-
interprete :: InterpreterContext -> IO(Stack RailType)
interprete ctxt = do
        result <- interpreteStep ctxt
        if not (isRight result) then do 
            putStrLn $ fromLeft result
            return $ Bottom
        else do
            let nCtxt@(Context _ _ _ stack _ _ finished) = fromRight result
            if not finished then interprete nCtxt
            else return stack

getFunctionCountByName :: FunctionName -> Programm -> Either String Int
getFunctionCountByName functionName programm= getFunctionCountByName' functionName programm 0
        where getFunctionCountByName' name [] _ = Left $ "FunctionCall: Unknown Function "++name
              getFunctionCountByName' name ((n,f):xs) a 
                    | n == functionName = Right a
                    | otherwise = getFunctionCountByName' name xs (succ a)

interpreteStep :: InterpreterContext -> IO (Either String InterpreterContext)
interpreteStep context@(Context programm fc ip workingStack functionCallStack variablesHashMap finished)
        | ip == start && (isNothing maybeNewLexeme || getLexemeName (fromJust maybeNewLexeme) /= "Start") = return $ Left "Function has to start with a $ sign"
        | isRail = return $ Right $ Context programm fc (step grid $ nextParseIP) workingStack functionCallStack variablesHashMap finished
        | isNothing maybeNewLexeme = return $ Left $ "Undefined char found"
        | getLexemeName (fromJust maybeNewLexeme) == "Constant" = return $ Right (Context programm fc (step grid nextParseIP) (stackPushConstant (fromJust maybeNewLexeme) workingStack) functionCallStack variablesHashMap finished)
        | getLexemeName (fromJust maybeNewLexeme) == "Pop" = let res = popVariable (fromJust maybeNewLexeme) workingStack variablesHashMap in if isRight res 
                                                                                                                                              then return $ Right (Context programm fc (step grid nextParseIP) (fst $ fromRight res) functionCallStack (snd $ fromRight res) finished)
                                                                                                                                              else return $ Left $ fromLeft res
        | getLexemeName (fromJust maybeNewLexeme) == "Push" = let res = pushVariable (fromJust maybeNewLexeme) workingStack variablesHashMap in if isRight res 
                                                                                                                                                then return $ Right (Context programm fc (step grid nextParseIP) (fromRight res) functionCallStack variablesHashMap finished)
                                                                                                                                                else return $ Left $ fromLeft res
        | getLexemeName (fromJust maybeNewLexeme) == "Add" = callRelatedFunction (stackMathAdd)
        | getLexemeName (fromJust maybeNewLexeme) == "Substract" = callRelatedFunction (stackMathSub)
        | getLexemeName (fromJust maybeNewLexeme) == "Multiply" = callRelatedFunction (stackMathMul)
        | getLexemeName (fromJust maybeNewLexeme) == "Divide" = callRelatedFunction (stackMathDiv)
        | getLexemeName (fromJust maybeNewLexeme) == "Remainder" = callRelatedFunction (stackMathMod)
        | getLexemeName (fromJust maybeNewLexeme) == "Size" = callRelatedFunction (stackStringSize)
        | getLexemeName (fromJust maybeNewLexeme) == "Append" = callRelatedFunction (stackStringAppend)
        | getLexemeName (fromJust maybeNewLexeme) == "Cut" = callRelatedFunction (stackStringCut)
        | getLexemeName (fromJust maybeNewLexeme) == "Output" = callRelatedIOFunction (systemOutput)
        | getLexemeName (fromJust maybeNewLexeme) == "Underflow" = callRelatedFunction (systemUnderflow)
        | getLexemeName (fromJust maybeNewLexeme) == "RType" = callRelatedFunction (systemType)
        | getLexemeName (fromJust maybeNewLexeme) == "Nil" = callRelatedFunction (stackListNil)
        | getLexemeName (fromJust maybeNewLexeme) == "Cons" = callRelatedFunction (stackListCons)
        | getLexemeName (fromJust maybeNewLexeme) == "Breakup" = callRelatedFunction (stackListBreakup)
        | getLexemeName (fromJust maybeNewLexeme) == "Equal" = callRelatedFunction (stackEqual)
        | getLexemeName (fromJust maybeNewLexeme) == "Greater" = callRelatedFunction (stackGreater)
        | getLexemeName (fromJust maybeNewLexeme) == "Call" = do 
                    res <- functionCall (fromJust maybeNewLexeme) programm workingStack functionCallStack
                    if isRight res
                    then return $ Right (Context programm fc (step grid nextParseIP) (fromRight res) functionCallStack variablesHashMap finished)
                    else return $ Left $ fromLeft res
        | getLexemeName (fromJust maybeNewLexeme) == "Finish" = return $ Right $ Context programm fc nextStepIP workingStack (pop functionCallStack) variablesHashMap True
        | getLexemeName (fromJust maybeNewLexeme) == "Junction" = let res = junctionDecision grid (nextParseIP) workingStack in if isRight res 
                                                                                                                                then return $ Right $ Context programm fc (step grid $ fst $fromRight res) (snd $ fromRight res) functionCallStack variablesHashMap finished
                                                                                                                                else return $ Left $ fromLeft res
        | otherwise =return $ Left $ "unknown" ++ show (fromJust maybeNewLexeme)
        where  isRail = elem currentChar "\\|-/x+*$"
               nextStepIP = step grid ip
               (maybeNewLexeme,nextParseIP) = parse grid ip
               currentChar = current grid ip
               grid = snd $ programm !! fc
               callRelatedFunction function = let newStack = function workingStack in if isRight newStack
                                                                                      then return $ Right (Context programm fc (step grid nextParseIP) (fromRight newStack) functionCallStack variablesHashMap finished)
                                                                                      else return $ Left (fromLeft newStack)
               callRelatedIOFunction function = do
                    newStack <- function workingStack
                    if isRight newStack
                    then return $ Right (Context programm fc (step grid nextParseIP) (fromRight newStack) functionCallStack variablesHashMap finished)
                    else return $ Left $ fromLeft newStack

toRailBool :: Bool -> RailType
toRailBool True = RailString "t"
toRailBool False = RailString "f"

stackGreater :: Stack RailType -> Either String (Stack RailType)
stackGreater (Elem (RailString a) (Elem (RailString b) rest))
        | isRight a' && isRight b' = Right $ push (toRailBool $ (fromRight a') < (fromRight b')) rest
        | isRight a' = Left $ fromLeft b'
        | isRight b' = Left $ fromLeft a'
        | otherwise = concatLefts a' b'
        where a' = toNumber a "Conditional: Top element is not an unsigned integer"
              b' = toNumber b "Conditional: Second element is not an unsigned integer"
stackGreater _ = Left "Conditional: invalid stack setting\n"

stackEqual :: Stack RailType -> Either String (Stack RailType)
stackEqual (Elem a (Elem b rest)) = Right $ push (toRailBool (a == b)) rest
stackEqual _ = Left "Conditional: invalid stack setting\n"

junctionDecision :: IDT.Grid2D -> IP -> Stack RailType -> Either String (IP,Stack RailType)
junctionDecision grid ip (Elem (RailString "t") rest) = Right $ (fst $ junctionturns grid ip,rest)
junctionDecision grid ip (Elem (RailString "f") rest) = Right $ (snd $ junctionturns grid ip,rest)
junctionDecision _ _ stack = Left $ "No Boolean on top" ++ show stack

functionCall :: IDT.Lexeme ->  Programm -> Stack RailType -> Stack FunctionName -> IO(Either String (Stack RailType))
functionCall (Call name) programm workingStack functionCallStack = do
        let newFc = getFunctionCountByName name programm
        if not (isRight newFc)
        then return $ Left $ fromLeft newFc
        else do
            result <- interprete (Context programm (fromRight newFc) start workingStack (push name functionCallStack) Map.empty False)
            return $ Right result


stackListBreakup :: Stack RailType -> Either String (Stack RailType)
stackListBreakup (Elem (RailList (Elem a listRest)) rest) = Right $ push a $ push (RailList listRest) rest
stackListBreakup (Elem _ _) = Left "List operation: No list for breakup found"
stackListBreakup _ = Left "List operation: Empty Stack\n"

stackListCons :: Stack RailType -> Either String (Stack RailType)
stackListCons (Elem a (Elem (RailList Bottom) rest)) = Right $ push (RailList $ Elem a Bottom) rest
stackListCons (Elem a (Elem (RailList list) rest)) | getRailTypeName a == (getRailTypeName $ top list) = Right $ push (RailList $ push a list) rest
stackListCons (Elem a (Elem _ _)) = Left "List operation: No list for cons found"
stackListCons _ = Left "List operation: invalid setting for cons\n"

stackListNil :: Stack RailType -> Either String (Stack RailType)
stackListNil stack = Right $ push (RailList Bottom) stack

systemOutput :: Stack RailType -> IO(Either String (Stack RailType))
systemOutput (Elem (RailString a) rest) = putStr (substituteEscapedChars a) >> return (Right $ rest)
systemOutput (Elem _ _) = return $ Left "System operation: unable to show list\n"
systemOutput Bottom = return $ Left "System operation: Empty Stack\n"

substituteEscapedChars :: String -> String
substituteEscapedChars [] = []
substituteEscapedChars ('\\':y:'\\':xs)
        | elem y "nt[]" = '\\':y: substituteEscapedChars xs
        | otherwise = '\\':y:'\\': substituteEscapedChars xs
substituteEscapedChars (x:xs) = x : substituteEscapedChars xs

stackStringCut :: Stack RailType -> Either String (Stack RailType)
stackStringCut (Elem (RailString a) (Elem (RailString b) rest)) = let nr = toNumber b "String operation: cut index is not an unsinged integer" in if not (isRight nr)
                                                                                                                                         then Left $ fromLeft nr
                                                                                                                                         else if (fromRight nr) >= 0 && (fromRight nr) <= (length a)
                                                                                                                                              then let (c,d) = splitAt (fromRight nr) a in Right $ push (RailString c) $ push (RailString d) rest
                                                                                                                                              else Left "String operation: cut index is out of range"

stackStringAppend :: Stack RailType -> Either String (Stack RailType)
stackStringAppend (Elem (RailString a) (Elem (RailString b) rest)) = Right $ push (RailString $ a ++ b) rest
stackStringAppend (Elem _ (Elem _ _)) = Left "String operation: 'append' does not work for lists"
stackStringAppend Bottom = Left "String operation: Empty Stack"

intToRailString :: Int -> RailType
intToRailString a = RailString $ show a

stackStringSize :: Stack RailType -> Either String (Stack RailType)
stackStringSize (Elem (RailString a) rest) = Right $ push (intToRailString $ length a) rest
stackStringSize (Elem _ _) = Left "String operation: 'size' does not work for lists"
stackStringSize Bottom = Left "String operation: Empty Stack"

stackPushConstant :: IDT.Lexeme -> Stack RailType -> Stack RailType
stackPushConstant constant workingStack = push (RailString $ fromConstant constant) workingStack

stackMathDiv :: Stack RailType -> Either String (Stack RailType)
stackMathDiv = stackMathDivLike (div)

stackMathMod :: Stack RailType -> Either String (Stack RailType)
stackMathMod = stackMathDivLike (mod)

stackMathDivLike :: (Int -> Int -> Int) -> Stack RailType -> Either String (Stack RailType)
stackMathDivLike function (Elem (RailString a) (Elem (RailString b) rest))
        | isRight first && isRight second = let secondDiv = fromRight second in if secondDiv /= 0
                                                                                then Right $ push (RailString $ show $ (function (fromRight first) secondDiv)) rest
                                                                                else Left "Math operation: Division through zero"
        | isRight first = Left $ fromLeft second
        | isRight second = Left $ fromLeft first
        | otherwise = concatLefts first second
        where first = toNumber a "Math operation: Top element of the stack is not an unsinged integer"
              second = toNumber b "Math operation: Second element of the stack is not an unsinged integer"
stackMathDivLike _  (Elem _ (Elem _ _)) = Left "Math operation: Operation is undefined for lists"
stackMathDivLike _ _ = Left "Math operation: Not enough arguments on the stack"

stackMathAdd :: Stack RailType -> Either String (Stack RailType)
stackMathAdd = stackArith (+)

stackMathSub :: Stack RailType -> Either String (Stack RailType)
stackMathSub = stackArith (-)

stackMathMul :: Stack RailType -> Either String (Stack RailType)
stackMathMul = stackArith (*)

stackArith :: (Int -> Int -> Int) -> Stack RailType -> Either String (Stack RailType)
stackArith function (Elem (RailString a) (Elem (RailString b) rest))
        | isRight firstSummand && isRight secondSummand = Right $ push (RailString $ show $ (function (fromRight firstSummand) (fromRight secondSummand))) rest
        | isRight firstSummand = Left $ fromLeft secondSummand
        | isRight secondSummand = Left $ fromLeft firstSummand
        | otherwise = concatLefts firstSummand secondSummand
        where firstSummand = toNumber a "Math operation: Top element of the stack is not an unsinged integer"
              secondSummand = toNumber b "Math operation: Second element of the stack is not an unsinged integer"
stackArith _ (Elem _ (Elem _ _)) = Left "Math operation: Operation is undefined for lists"
stackArith _ _ = Left "Math operation: Not enough arguments on the stack"

concatLefts :: Either String a -> Either String a -> Either String b
concatLefts (Left a) (Left b) = Left $ a ++ "\n" ++ b

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "Right"

fromRight :: Either a b -> b
fromRight (Right a) = a
fromRight a = error $ "left"

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

toNumber :: String -> String -> Either String Int
toNumber a errorMsg | foldl1 (&&) $ Prelude.map (\z -> elem z "0123456789") a = Right  (read a :: Int)
                    | otherwise = Left errorMsg
-}
