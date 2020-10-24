module Interpreter where

import Code
import Control.Monad.Except
import Control.Monad.Trans.State
import Data.Map.Strict as M
import Parser.PascalGrammar

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
  | NotSupportedOperation String PascalTypedValue
  deriving (Show)

type FunctionValue = (PascalType, Code)

type FunctionKey = (Identifier, [PascalType])

type FunctionData = M.Map FunctionKey FunctionValue

type VariableEntry = (PascalTypedValue, PascalType)

type Scope = (M.Map String VariableEntry)

type Store = (FunctionData, [Scope])

type Interpreter = StateT Store (Except Err)

createFunctionKey :: Identifier -> [Parameter] -> FunctionKey
createFunctionKey ident params = (ident, concatMap (\p -> replicate (length $ paramIdents p) $ paramType p) params)

-- puts function definition to store
defineFunction :: FunctionKey -> FunctionValue -> Interpreter ()
defineFunction funcKey funcValue = do
  (fd, scope) <- get
  if M.notMember funcKey fd
    then put (M.insert funcKey funcValue fd, scope)
    else throwError $ AmbiguousFunction funcKey

-- returns function from store
getFunction :: FunctionKey -> Interpreter FunctionValue
getFunction key = do
  (fd, _) <- get
  case M.lookup key fd of
    Nothing -> throwError $ UndefinedFunction key
    Just res -> return res

-- puts variable declaration to topmost scope
declareVar :: Identifier -> PascalType -> Interpreter ()
declareVar ident vType = modify $ \(fd, vd : vds) -> (fd, M.insert ident (EmptyValue, vType) vd : vds)

getVar :: Identifier -> Interpreter VariableEntry
getVar ident = do
  (_, scope) <- get
  let var = msum $ M.lookup ident <$> scope
  case var of
    Nothing -> throwError $ UndeclaredVariable ident
    Just varEntry -> return varEntry

-- assigns the topmost variable a given expression
assignVar :: Identifier -> PascalTypedValue -> Interpreter ()
assignVar ident value = do
  (fd, scope) <- get
  (_, vType) <- getVar ident
  let (f, s) = break (M.member ident) scope
  case s of
    [] -> throwError $ UndeclaredVariable ident
    (sc : scs) ->
      if getType value == vType
        then put (fd, f ++ M.insert ident (value, vType) sc : scs)
        else throwError $ TypeCastErr vType $ getType value

-- returns the topmost variable
getDefinedVar :: Identifier -> Interpreter VariableEntry
getDefinedVar ident = do
  (value, vType) <- getVar ident
  case value of
    EmptyValue -> throwError $ UndefinedVariable ident
    _ -> return (value, vType)

unaryOp :: UnaryOperation -> PascalTypedValue -> Interpreter PascalTypedValue
unaryOp Not (BooleanValue b) = return $ BooleanValue $ not b
unaryOp Negate (IntegerValue i) = return $ IntegerValue $ negate i
unaryOp Negate (RealValue r) = return $ RealValue $ negate r

--todo
--unaryOp op val = throwError $ NotSupportedOperation $ show op $ val

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
eval (ExprFunctionCall ident paramList) = do
  (fd, scope) <- get
  params <- sequence $ eval <$> paramListParams paramList
  let funcKey = (ident, getType <$> params)
  fun <- getFunction funcKey
  --  todo
  undefined

interpret :: Code -> Interpreter ()
interpret (Free (PrintLn str next)) = undefined
interpret (Free (ReadString f)) = undefined
interpret (Free (Assign ident expr next)) = do
  exprResult <- eval expr
  assignVar ident exprResult
  interpret next
interpret (Free (While expr code next)) = do
  res <- eval expr
  case res of
    --    todo сперва выполняю code затем иду опять выполянть это
    (BooleanValue True) -> undefined
    (BooleanValue False) -> interpret next
    _ -> throwError $ TypeCastErr PascalBoolean $ getType res
interpret (Free (If expr succCode unsuccCode next)) = do
  res <- eval expr
  case res of
    (BooleanValue True) -> interpret succCode
    (BooleanValue False) -> interpret unsuccCode
    _ -> throwError $ TypeCastErr PascalBoolean $ getType res
  interpret next
interpret (Free (Function ident paramSection returnType body next)) = do
  let key = createFunctionKey ident $ parameterSectionParams paramSection
  defineFunction key (returnType, body)
  interpret next
interpret (Free (Expression expr next)) = eval expr >> interpret next
interpret (Free (VDeclaration ident vType next)) = declareVar ident vType >> interpret next
--interpret (Free (For ident exprFrom increment exprTo code next))
interpret (Free (Empty next)) = interpret next
