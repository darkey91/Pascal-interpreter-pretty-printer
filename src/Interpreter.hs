{-# LANGUAGE LambdaCase #-}

module Interpreter (run) where

import Code
import Control.Exception (Exception, throwIO)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT, modify, msum, evalStateT)
import Data.Map.Strict as M
import Parser.PascalGrammar
import Utils

type ExpectedType = PascalType

type ActualType = PascalType

data Err
  = LogicErr
  | InterpretErr
  | TypeCastErr ExpectedType ActualType
  | UndeclaredVariable String
  | UndefinedVariable String
  | UndefinedFunction FunctionKey
  | AmbiguousFunction FunctionKey
  | UnexpectedError String
  | NotSupportedOperation UnaryOperation PascalTypedValue
  | TypesNotMatch BinaryOperation PascalTypedValue PascalTypedValue
  deriving (Show)

instance Exception Err

type FunctionKey = (Identifier, [PascalType])

type FunctionValue = ([Parameter], PascalType, Code)

type FunctionData = M.Map FunctionKey FunctionValue

type VariableEntry = (PascalTypedValue, PascalType)

type Scope = M.Map Identifier VariableEntry

type TemporaryValue = (Identifier, PascalTypedValue)

type Store = (FunctionData, [Scope], [TemporaryValue])

type Interpreter = StateT Store IO

createFunctionKey :: Identifier -> [Parameter] -> FunctionKey
createFunctionKey ident params = (ident, concatMap (\p -> replicate (length $ paramIdents p) $ paramType p) params)

-- puts function definition to store
defineFunction :: FunctionKey -> FunctionValue -> Interpreter ()
defineFunction funcKey funcValue = do
  (fd, scope, temp) <- get
  if M.notMember funcKey fd
    then put (M.insert funcKey funcValue fd, scope, temp)
    else liftIO $ throwIO $ AmbiguousFunction funcKey

-- returns function from store
getFunction :: FunctionKey -> Interpreter FunctionValue
getFunction key = do
  (fd, _, _) <- get
  case M.lookup key fd of
    Nothing -> liftIO $ throwIO $ UndefinedFunction key
    Just res -> return res

-- puts variable declaration to topmost scope
declareVar :: Identifier -> PascalType -> Interpreter ()
declareVar ident vType = modify $ \(fd, vd : vds, temp) -> (fd, M.insert ident (EmptyValue, vType) vd : vds, temp)

getVar :: Identifier -> Interpreter VariableEntry
getVar ident = do
  (_, scope, _) <- get
  let var = msum $ M.lookup ident <$> scope
  case var of
    Nothing -> liftIO $ throwIO $ UndeclaredVariable ident
    Just varEntry -> return varEntry

-- assigns the topmost variable a given expression
assignVar :: Identifier -> PascalTypedValue -> Interpreter ()
assignVar ident value = do
  (fd, scope, temp) <- get
  (_, vType) <- getVar ident
  let (f, s) = break (M.member ident) scope
  case s of
    [] -> liftIO $ throwIO $ UndeclaredVariable ident
    (sc : scs) ->
      if getType value == vType
        then put (fd, f ++ M.insert ident (value, vType) sc : scs, temp)
        else liftIO $ throwIO $ TypeCastErr vType $ getType value

-- returns the topmost variable
getDefinedVar :: Identifier -> Interpreter VariableEntry
getDefinedVar ident = do
  (value, vType) <- getVar ident
  case value of
    EmptyValue -> liftIO $ throwIO $ UndefinedVariable ident
    _ -> return (value, vType)

unaryOp :: UnaryOperation -> PascalTypedValue -> Interpreter PascalTypedValue
unaryOp Not (BooleanValue b) = return $ BooleanValue $ not b
unaryOp Negate (IntegerValue i) = return $ IntegerValue $ negate i
unaryOp Negate (RealValue r) = return $ RealValue $ negate r
unaryOp op val = liftIO $ throwIO $ NotSupportedOperation op val

binaryOp :: BinaryOperation -> PascalTypedValue -> PascalTypedValue -> Interpreter PascalTypedValue
binaryOp Plus (IntegerValue l) (IntegerValue r) = return $ IntegerValue $ l + r
binaryOp Plus (RealValue l) (RealValue r) = return $ RealValue $ l + r
binaryOp Plus (StringValue l) (StringValue r) = return $ StringValue $ l ++ r
binaryOp Minus (IntegerValue l) (IntegerValue r) = return $ IntegerValue $ l - r
binaryOp Minus (RealValue l) (RealValue r) = return $ RealValue $ l - r
binaryOp Mul (IntegerValue l) (IntegerValue r) = return $ IntegerValue $ l * r
binaryOp Mul (RealValue l) (RealValue r) = return $ RealValue $ l * r
binaryOp IntDiv (IntegerValue l) (IntegerValue r) = return $ IntegerValue $ l `div` r
binaryOp Div (RealValue l) (RealValue r) = return $ RealValue $ l / r
binaryOp And (BooleanValue l) (BooleanValue r) = return $ BooleanValue $ l && r
binaryOp Or (BooleanValue l) (BooleanValue r) = return $ BooleanValue $ l || r
binaryOp EqOp (IntegerValue l) (IntegerValue r) = return $ BooleanValue $ l == r
binaryOp EqOp (RealValue l) (RealValue r) = return $ BooleanValue $ l == r
binaryOp EqOp (StringValue l) (StringValue r) = return $ BooleanValue $ l == r
binaryOp EqOp (BooleanValue l) (BooleanValue r) = return $ BooleanValue $ l == r
binaryOp NeqOp l r = do
  result <- binaryOp EqOp l r
  unaryOp Not result
binaryOp GTOp (IntegerValue l) (IntegerValue r) = return $ BooleanValue $ l > r
binaryOp GTOp (RealValue l) (RealValue r) = return $ BooleanValue $ l > r
binaryOp GTOp (StringValue l) (StringValue r) = return $ BooleanValue $ l > r
binaryOp GTOp (BooleanValue l) (BooleanValue r) = return $ BooleanValue $ l > r
binaryOp LTOp l r = do
  neq <- binaryOp NeqOp l r
  gt <- binaryOp GTOp r l
  binaryOp And neq gt
binaryOp GEOp l r = do
  eq <- binaryOp EqOp l r
  gt <- binaryOp GTOp l r
  binaryOp Or eq gt
binaryOp LEOp l r = do
  eq <- binaryOp EqOp l r
  lt <- binaryOp LTOp r l
  binaryOp Or eq lt
binaryOp op l r = liftIO $ throwIO $ TypesNotMatch op l r

eval :: Expr -> Interpreter PascalTypedValue
eval (ExprBracketed expr) = eval expr
eval (ExprVal typedValue) = return typedValue
eval (ExprVar ident) = do
  (value, _) <- getDefinedVar ident
  return value
eval (ExprBinOp binOp lhs rhs) = do
  l <- eval lhs
  r <- eval rhs
  binaryOp binOp l r
eval (ExprUnOp unOp expr) = do
  e <- eval expr
  unaryOp unOp e
eval (ExprFunctionCall ident argList) = do
  (fd, scope, temp) <- get
  argValues <- sequence $ eval <$> paramListParams argList
  let argTypes = getType <$> argValues
  let funcKey = (ident, argTypes)
  (params, rType, body) <- getFunction funcKey
  let newScope = M.fromList $ zip (concatMap paramIdents params) (zip argValues argTypes)
  case rType of
    PascalVoid -> put (fd, newScope : scope, temp) >> interpret body >> return EmptyValue
    _ -> do
      put (fd, newScope : scope, (ident, EmptyValue) : temp)
      interpret body
      (fd', scope', temp') <- get
      case temp' of
        [] -> liftIO $ throwIO $ UnexpectedError "function call"
        ((_, rVal) : tmp) -> put (fd', scope', tmp) >> (getType rVal == rType ? return rVal :? liftIO (throwIO $ TypeCastErr rType $getType rVal))

interpret :: Code -> Interpreter ()
interpret (Free (Writeln exprs next)) = do
  values <- sequence $ eval <$> exprs
  liftIO $ putStrLn $ concatMap show values
  interpret next
interpret (Free (Readln idents next)) = do
  varDecls <- sequence $ getVar <$> idents
  values <- sequence $ readValueIO <$> (snd <$> varDecls)
  sequence_ $ uncurry assignVar <$> zip idents values
  interpret next
interpret (Free (Assign ident expr next)) = do
  exprResult <- eval expr
  assignVar ident exprResult
  interpret next
interpret block@(Free (While expr body next)) = do
  res <- eval expr
  case res of
    (BooleanValue True) -> interpret body >> interpret block
    (BooleanValue False) -> interpret next
    _ -> liftIO $ throwIO $ TypeCastErr PascalBoolean $ getType res
interpret (Free (If expr succCode unsuccCode next)) = do
  res <- eval expr
  case res of
    (BooleanValue True) -> interpret succCode
    (BooleanValue False) -> interpret unsuccCode
    _ -> liftIO $ throwIO $ TypeCastErr PascalBoolean $ getType res
  interpret next
interpret (Free (Function ident paramSection returnType body next)) = do
  let params = parameterSectionParams paramSection
  let key = createFunctionKey ident params
  defineFunction key (params, returnType, body)
  interpret next
interpret (Free (Expression expr next)) = eval expr >> interpret next
interpret (Free (VDeclaration ident vType next)) = declareVar ident vType >> interpret next
interpret (Free (Empty next)) = interpret next
interpret (Free (For ident exprFrom increment exprTo body next)) = do
  to <- eval exprTo
  from <- eval exprFrom
  case increment of
    To -> forStmtInterpHelper ident from to body Plus
    DownTo -> forStmtInterpHelper ident from to body Minus
  interpret next
interpret (Pure _) = return ()

readValueIO :: PascalType -> Interpreter PascalTypedValue
readValueIO = \case
  PascalString -> liftIO $ StringValue <$> readLn
  PascalInteger -> liftIO $ IntegerValue <$> readLn
  PascalReal -> liftIO $ RealValue <$> readLn
  PascalBoolean -> liftIO $ BooleanValue <$> readLn
  PascalVoid -> liftIO $ throwIO $ UnexpectedError "variable with void type"

forStmtInterpHelper :: Identifier -> PascalTypedValue -> PascalTypedValue -> Code -> BinaryOperation -> Interpreter ()
forStmtInterpHelper ident from to body binOp = do
  assignVar ident from
  cond <- binaryOp GTOp from to
  newIterVal <- binaryOp binOp from $ IntegerValue 1
  if toBool cond
    then return ()
    else interpret body >> forStmtInterpHelper ident newIterVal to body binOp

toBool :: PascalTypedValue -> Bool
toBool (BooleanValue b) = b
toBool _ = False

run :: Program -> IO ()
run prog = do
  _ <- evalStateT (interpret $ convert $ programBlock prog) (M.empty, [M.empty], [])
  return ()
