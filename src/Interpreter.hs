{-# LANGUAGE LambdaCase #-}

module Interpreter (run) where

import Code
import Control.Exception (Exception, throwIO)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT, evalStateT, modify,
                            msum)
import Data.Map.Strict as M
import Parser.PascalGrammar

import Data.List (intercalate)

type ExpectedType = PascalType

type ActualType = PascalType

data Err
  = TypeCastErr ExpectedType ActualType String
  | UndeclaredVariable String
  | UndefinedVariable String
  | UndefinedFunction FunctionKey
  | AmbiguousFunction FunctionKey
  | UnexpectedError String
  | NotSupportedOperation UnaryOperation PascalType
  | TypesNotMatch BinaryOperation PascalTypedValue PascalTypedValue
  | MultipleDeclaration Identifier
  | NoValueReturned Identifier [Parameter]

instance Show Err where
  show (TypeCastErr expType actType str) = str ++ "Type doesn't match. Expected: " ++ show expType ++ ", actual: " ++ show actType
  show (UndeclaredVariable ident) = "Undeclared variable: " ++ ident
  show (UndefinedVariable ident) = "Undefined variable: " ++ ident
  show (UndefinedFunction (fIdent, pTypes)) = concat ["Undefined function: ", fIdent, "(" , intercalate ", " $ show <$> pTypes, ")"]
  show (AmbiguousFunction (fIdent, pTypes)) = concat ["Ambiguous function definition: ", fIdent, "(" , intercalate ", " $ show <$> pTypes, ")"]
  show (UnexpectedError str) = "UnexpectedError: " ++ str
  show (NotSupportedOperation unOp pType) = "Unary operation " ++ show unOp ++ " is not supported for " ++ show pType
  show (TypesNotMatch binOp lVal rVal) = concat ["Type mismatch: ", show lVal, show binOp, show rVal ]
  show (MultipleDeclaration ident) = "Multiple declaration of variable " ++ ident
  show (NoValueReturned fIdent params) = do
    let pTypes = show <$> snd (createFunctionKey fIdent params)
    "No value returned from function " ++ fIdent ++ "(" ++ intercalate "," pTypes ++  ")"

instance Exception Err

type FunctionKey = (Identifier, [PascalType])

type FunctionValue = ([Parameter], PascalType, Code)

type FunctionData = M.Map FunctionKey FunctionValue

type VariableEntry = (PascalTypedValue, PascalType)

type Scope = M.Map Identifier VariableEntry

type Store = (FunctionData, [Scope])

type Interpreter = StateT Store IO

createFunctionKey :: Identifier -> [Parameter] -> FunctionKey
createFunctionKey ident params = (ident, concatMap (\p -> replicate (length $ paramIdents p) $ paramType p) params)

defineFunction :: FunctionKey -> FunctionValue -> Interpreter ()
defineFunction funcKey funcValue = do (fd, scope) <- get
                                      if M.notMember funcKey fd
                                      then put (M.insert funcKey funcValue fd, scope)
                                      else liftIO $ throwIO $ AmbiguousFunction funcKey

getFunction :: FunctionKey -> Interpreter FunctionValue
getFunction key  = do (fd, _) <- get
                      case M.lookup key fd of
                        Nothing  -> liftIO $ throwIO $ UndefinedFunction key
                        Just res -> return res

declareVar :: Identifier -> PascalType -> Interpreter ()
declareVar ident vType = do (fd, v : vs) <- get
                            if M.member ident v
                            then liftIO $ throwIO $ MultipleDeclaration ident
                            else put (fd, M.insert ident (EmptyValue, vType) v : vs)

getVar :: Identifier -> Interpreter VariableEntry
getVar ident = do (_, scope) <- get
                  let var = msum $ M.lookup ident <$> scope
                  case var of
                    Nothing       -> liftIO $ throwIO $ UndeclaredVariable ident
                    Just varEntry -> return varEntry

assignVar :: Identifier -> PascalTypedValue -> Interpreter ()
assignVar ident value  = do (fd, scope) <- get
                            (_, vType) <- getVar ident
                            let (f, s) = break (M.member ident) scope
                            case s of
                              [] -> liftIO $ throwIO $ UndeclaredVariable ident
                              (sc : scs) -> if getType value == vType
                                            then put (fd, f ++ (M.insert ident (value, vType) sc : scs))
                                            else liftIO $ throwIO $ TypeCastErr vType (getType value) ("Can't assign variable " ++ ident ++ ". ")

getDefinedVar :: Identifier -> Interpreter VariableEntry
getDefinedVar ident  = do (value, vType) <- getVar ident
                          case value of
                            EmptyValue -> liftIO $ throwIO $ UndefinedVariable ident
                            _          -> return (value, vType)
unaryOp :: UnaryOperation -> PascalTypedValue -> Interpreter PascalTypedValue
unaryOp Not (BooleanValue b)    = return $ BooleanValue $ not b
unaryOp Negate (IntegerValue i) = return $ IntegerValue $ negate i
unaryOp Negate (RealValue r)    = return $ RealValue $ negate r
unaryOp op val                  = liftIO $ throwIO $ NotSupportedOperation op $ getType val

binaryOp :: BinaryOperation -> PascalTypedValue -> PascalTypedValue -> Interpreter PascalTypedValue
binaryOp Plus (IntegerValue l) (IntegerValue r)   = return $ IntegerValue $ l + r
binaryOp Plus (RealValue l) (RealValue r)         = return $ RealValue $ l + r
binaryOp Plus (StringValue l) (StringValue r)     = return $ StringValue $ l ++ r
binaryOp Minus (IntegerValue l) (IntegerValue r)  = return $ IntegerValue $ l - r
binaryOp Minus (RealValue l) (RealValue r)        = return $ RealValue $ l - r
binaryOp Mul (IntegerValue l) (IntegerValue r)    = return $ IntegerValue $ l * r
binaryOp Mul (RealValue l) (RealValue r)          = return $ RealValue $ l * r
binaryOp IntDiv (IntegerValue l) (IntegerValue r) = return $ IntegerValue $ l `div` r
binaryOp Div (RealValue l) (RealValue r)          = return $ RealValue $ l / r
binaryOp And (BooleanValue l) (BooleanValue r)    = return $ BooleanValue $ l && r
binaryOp Or (BooleanValue l) (BooleanValue r)     = return $ BooleanValue $ l || r
binaryOp EqOp (IntegerValue l) (IntegerValue r)   = return $ BooleanValue $ l == r
binaryOp EqOp (RealValue l) (RealValue r)         = return $ BooleanValue $ l == r
binaryOp EqOp (StringValue l) (StringValue r)     = return $ BooleanValue $ l == r
binaryOp EqOp (BooleanValue l) (BooleanValue r)   = return $ BooleanValue $ l == r
binaryOp GTOp (IntegerValue l) (IntegerValue r)   = return $ BooleanValue $ l > r
binaryOp GTOp (RealValue l) (RealValue r)         = return $ BooleanValue $ l > r
binaryOp GTOp (StringValue l) (StringValue r)     = return $ BooleanValue $ l > r
binaryOp GTOp (BooleanValue l) (BooleanValue r)   = return $ BooleanValue $ l > r
binaryOp NeqOp l r = do result <- binaryOp EqOp l r
                        unaryOp Not result
binaryOp LTOp l r  = do neq <- binaryOp NeqOp l r
                        gt <- binaryOp GTOp r l
                        binaryOp And neq gt
binaryOp GEOp l r  = do eq <- binaryOp EqOp l r
                        gt <- binaryOp GTOp l r
                        binaryOp Or eq gt
binaryOp LEOp l r  = do eq <- binaryOp EqOp l r
                        lt <- binaryOp LTOp r l
                        binaryOp Or eq lt
binaryOp op l r    = liftIO $ throwIO $ TypesNotMatch op l r

eval :: Expr -> Interpreter PascalTypedValue
eval (ExprBracketed expr)      = eval expr
eval (ExprVal typedValue)      = return typedValue
eval (ExprVar ident)           = do (value, _) <- getDefinedVar ident
                                    return value
eval (ExprBinOp binOp lhs rhs) = do l <- eval lhs
                                    r <- eval rhs
                                    binaryOp binOp l r
eval (ExprUnOp unOp expr)      = do e <- eval expr
                                    unaryOp unOp e
eval (ExprFunctionCall fIdent argList) = do (params, _, body) <- prepareStoreForFuncCall fIdent argList True
                                            interpret body
                                            (retValue, _) <- getVar fIdent
                                            modify $ \(fd, _:ss) -> (fd, ss)
                                            case retValue of
                                              EmptyValue -> liftIO $ throwIO $ NoValueReturned fIdent params
                                              _ -> return retValue

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
    (BooleanValue True)  -> interpret body >> interpret block
    (BooleanValue False) -> interpret next
    _                    -> liftIO $ throwIO $ TypeCastErr PascalBoolean (getType res) ""
interpret (Free (If expr succCode unsuccCode next)) = do
  res <- eval expr
  case res of
    (BooleanValue True)  -> interpret succCode
    (BooleanValue False) -> interpret unsuccCode
    _                    -> liftIO $ throwIO $ TypeCastErr PascalBoolean  (getType res)  ""
  interpret next
interpret (Free (Function ident paramSection returnType body next)) = do
  let params = parameterSectionParams paramSection
  let key = createFunctionKey ident params
  defineFunction key (params, returnType, body)
  interpret next
interpret (Free (VDeclaration ident vType next)) = declareVar ident vType >> interpret next
interpret (Free (Empty next)) = interpret next
interpret (Free (ProcedureCall ident paramList next)) = do
  (_, _, body) <- prepareStoreForFuncCall ident paramList False
  interpret body
  modify $ \(fd, _:ss) -> (fd, ss)
  interpret next
interpret (Free (For ident exprFrom increment exprTo body next)) = do
  to   <- eval exprTo
  from <- eval exprFrom
  case increment of
    To     -> forStmtInterpHelper ident from to body Plus
    DownTo -> forStmtInterpHelper ident from to body Minus
  interpret next
interpret (Pure _) = return ()

readValueIO :: PascalType -> Interpreter PascalTypedValue
readValueIO = \case
  PascalString -> liftIO $ do l <- getLine
                              return (StringValue l)
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
toBool _                = False

prepareStoreForFuncCall :: Identifier -> ParamList -> Bool -> Interpreter FunctionValue
prepareStoreForFuncCall ident argList isFunction = do (fd, scope) <- get
                                                      argValues <- sequence $ eval <$> paramListParams argList
                                                      let argTypes = getType <$> argValues
                                                      funcValue@(params, rType, _) <- getFunction (ident, argTypes)
                                                      let newScope = zip (concatMap paramIdents params) (zip argValues argTypes)
                                                      if isFunction
                                                      then put (fd, M.fromList ((ident, (EmptyValue, rType)) : newScope) : scope)
                                                      else put (fd, M.fromList newScope : scope)
                                                      return funcValue

--- interprets given ast
run :: Program -> IO ()
run prog = do _ <- evalStateT (interpret $ convert $ programBlock prog) (M.empty, [M.empty])
              return ()
