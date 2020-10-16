--{-# LANGUAGE GADTs #-}

module Parser.PascalGrammar
  ( Program(..)
  , Block(..)
  , CompoundStatement
  , Statements
  , Statement(..)
  , Constant(..)
  , ConstantDefParts
  , ConstantDefPart
  , VarDeclParts
  , VarDeclPart
  , VariableDeclaration(..)
  , ProcedureAndFunctionDeclParts
  , ProcedureOrFunctionDeclaration(..)
  , ParameterSection
  , Parameter
  , PascalType(..)
  , PascalTypedValue(..)
  , Increment(..)
  , Expr(..)
  )
where

data Program = Program
  { programIdent :: String,
    programBlock :: Block
  }

data Block
  = SimpleBlock CompoundStatement
  | BlockWithConst ConstantDefParts Block
  | BlockWithVar VarDeclParts Block
  | BlockWithFunc ProcedureAndFunctionDeclParts Block

type CompoundStatement = Statements

type ConstantDefParts = [ConstantDefPart]

type ConstantDefPart = [Constant]

data Constant = Constant
  { constIdent :: VariableIdent,
    constValue :: PascalTypedValue,
    isConstVar :: Bool,
    isConstNeg :: Bool
  }

type VarDeclParts = [VarDeclPart]

type VarDeclPart = [VariableDeclaration]

data VariableDeclaration = VariableDeclaration
  { variableIdents :: [VariableIdent],
    variableType :: PascalType
  }

type ProcedureAndFunctionDeclParts = [ProcedureOrFunctionDeclaration]

data ProcedureOrFunctionDeclaration = ProcedureOrFunctionDeclaration
  { functionIdent :: Identifier,
    functionParameters :: ParameterSection,
    functionReturnType :: PascalType
  }

type ParameterSection = [Parameter]

type Parameter = VariableDeclaration

data PascalType = PascalString | PascalInteger | PascalReal | PascalBoolean | PascalChar | PascalVoid

data PascalTypedValue
  = PascalStringValue String
  | PascalIntegerValue Int
  | PascalRealValue Double
  | PascalBooleanValue Bool
  | PascalCharValue Char

type Statements = [Statement]

type SuccessfulCase = Statement

type UnsuccessfulCase = Statement

data Statement
  = AssignmentStmt Identifier Expr
  | ProcedureStmt Identifier ParameterSection
  | CompoundStmt CompoundStatement
  | IfStatement Expr SuccessfulCase UnsuccessfulCase
  | ForStatement Identifier Expr Increment Expr Statement
  | WhileStatement Expr Statement
  | RepeatStatement Statements Expr
  | EmptyStatement

data Increment = To | DownTo

type VariableIdent = Identifier

type Identifier = String

data Expr
  = ExprBracketed Expr
  | ExprVal PascalTypedValue
  | ExprNeg Expr
  | ExprPlus Expr Expr
  | ExprMinus Expr Expr
  | ExprMul Expr Expr
  | ExprDiv Expr Expr
  | ExprIntDiv Expr Expr
  | ExprEq Expr Expr
  | ExprNeq Expr Expr
  | ExprGT Expr Expr
  | ExprLT Expr Expr
  | ExprGE Expr Expr
  | ExprLE Expr Expr
  | ExprAnd Expr Expr
  | ExprOr Expr Expr
  | ExprNot Expr
  | ExprVar String
  | ExprFunctionDesignator String ParameterSection

--todo think about concatenation
--data Expr a where
--  ExprBracketed :: Expr a -> Expr a
--  ExprNumVal :: (Num a) => PascalTypedValue a -> Expr a
--  ExprBoolVal :: Bool -> Expr Bool
--  ExprNeg :: (Num a) => Expr a -> Expr a
--  ExprPlus :: (Num a) => Expr a -> Expr a -> Expr a
--  ExprMinus :: (Num a) => Expr a -> Expr a -> Expr a
--  ExprMul :: (Num a) => Expr a -> Expr a -> Expr a
--  ExprDiv :: (Fractional a) => Expr a -> Expr a -> Expr a
--  ExprIntDiv :: (Integral a) => Expr a -> Expr a -> Expr a
--  ExprAnd :: Expr Bool -> Expr Bool -> Expr Bool
--  ExprOr :: Expr Bool -> Expr Bool -> Expr Bool
--  ExprNot :: Expr Bool -> Expr Bool

--newtype Interp a = Interp { runInterp :: Store -> Either String (a, Store) }
--
--instance Monad Interp where
--  return x = Interp $ \r -> Right (x, r)
--  i >>= k  = Interp $ \r -> case runInterp i r of
--               Left msg      -> Left msg
--               Right (x, r') -> runInterp (k x) r'
--  fail msg = Interp $ \_ -> Left msg


--		| IDENTIFIER '=' IDENTIFIER					{ Constant $1 $3 True False }
--  	| IDENTIFIER '=' '-' IDENTIFIER				{ Constant $1 $4 True True}