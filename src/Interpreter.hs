module Interpreter where

import Code
--import Control.Monad.Trans.ExceptT
import Control.Monad.Except
import Control.Monad.Trans.State
import Data.Map.Strict as M
import Parser.PascalGrammar

data Err
  = LogicErr
  | InterpretErr
  | TypeCastErr
  | UndeclaredVariable String
  | UndefinedVariable String
  | UndefinedFunction String
  | AmbiguousFunction FunctionKey
  deriving (Eq, Show)

type FunctionValue = (PascalType, Code)

type FunctionKey = (Identifier, [Parameter])

type FunctionData = M.Map FunctionKey FunctionValue

type VariableEntry = (Expr, PascalType)

type Scope = (M.Map String VariableEntry)

type Store = (FunctionData, [Scope])

type Interpreter = StateT Store (Except Err)

--newtype StateT s (m :: * -> *) a
--  = StateT { runStateT :: s -> m (a, s) }

--newtype ExceptT e (m :: * -> *) a = ExceptT (m (Either e a))

defineFunction :: FunctionKey -> FunctionValue -> Interpreter ()
defineFunction funcKey funcValue = do
  store <- get
  let (fd, scope) = store
  if M.notMember funcKey fd
    then put (M.insert funcKey funcValue fd, scope)
    else throwError $ AmbiguousFunction funcKey

declareVar :: Identifier -> PascalType -> Interpreter ()
declareVar ident vType = modify $ \(fd, vd : vds) -> (fd, M.insert ident (EmptyExpr, vType) vd : vds)

assignVar :: Identifier -> PascalType -> Expr -> Interpreter ()
assignVar ident vType expr = modify $ \(fd, vd : vds) -> (fd, M.insert ident (expr, vType) vd : vds)

getDefinedVar :: Identifier -> Interpreter VariableEntry
getDefinedVar ident = do
  store <- get
  let scope = snd store
  let var = msum $ M.lookup ident <$> scope
  case var of
    Nothing -> throwError $ UndeclaredVariable ident
    --todo never lookup not assigned variable
    Just (EmptyExpr, _) -> throwError $ UndefinedVariable ident
    Just varEntry -> return varEntry

--todo remove
class Computable a where
  eval :: a -> Interpreter PascalTypedValue

--instance Computable Expr where
--  eval (ExprBracketed expr) = eval expr
--  eval (ExprVal typedValue) = return typedValue
--  eval (ExprNeg expr) = undefined
--  --  todo define through common function
--  eval (ExprPlus lhs rhs) =
--  eval (ExprMinus lhs rhs)
--  eval (ExprMul lhs rhs)
--  eval (ExprDiv lhs rhs)
--  eval (ExprIntDiv lhs rhs)
--  eval (ExprEq lhs rhs)
--  eval (ExprNeq lhs rhs)
--  eval (ExprGT lhs rhs)
--  eval (ExprLT lhs rhs)
--  eval (ExprGE lhs rhs)
--  eval (ExprLE lhs rhs)
--  eval (ExprAnd lhs rhs)
--  eval (ExprOr lhs rhs)
--  eval (ExprNot expr)
--  eval (ExprVar str)
--  eval (ExprFunctionCall ident paramList)
--  eval EmptyExpr = return EmptyValue