--{-# LANGUAGE GADTs #-}

module Parser.PascalGrammar
  ( Program (..),
    Block (..),
    Statements(..),
    Statement (..),
    ParamList(..),
    Constant (..),
    ConstantDefParts,
    ConstantDefPart(..),
    VarDeclParts,
    VarDeclPart(..),
    VariableDeclaration (..),
    ProcedureAndFunctionDeclParts,
    ProcedureOrFunctionDeclaration (..),
    ParameterSection(..),
    Parameter(..),
    PascalType (..),
    PascalTypedValue (..),
    Increment (..),
    Expr (..),
    isCompound,
    isEmpty
  )
where

data Program = Program
  { programIdent :: Identifier,
    programBlock :: Block
  }
  deriving (Eq, Show)

data Block
  = SimpleBlock Statements
  | BlockWithConst ConstantDefParts Block
  | BlockWithVar VarDeclParts Block
  | BlockWithFunc ProcedureAndFunctionDeclParts Block
  deriving (Eq, Show)

type ConstantDefParts = [ConstantDefPart]

newtype ConstantDefPart = ConstantDefPart { constantDefPartDefs :: [Constant] }
  deriving (Eq, Show)

data Constant = Constant
  { constIdent :: VariableIdent,
    constValue :: PascalTypedValue,
    isConstVar :: Bool,
    isConstNeg :: Bool
  }
  deriving (Eq, Show)

type VarDeclParts = [VarDeclPart]

newtype VarDeclPart = VarDeclPart { varDeclPartDecls :: [VariableDeclaration] }
  deriving (Eq, Show)

data VariableDeclaration = VariableDeclaration
  { variableDeclarationIdents :: [VariableIdent],
    variableDeclarationType :: PascalType
  }
  deriving (Eq, Show)

type ProcedureAndFunctionDeclParts = [ProcedureOrFunctionDeclaration]

data ProcedureOrFunctionDeclaration = ProcedureOrFunctionDeclaration
  { functionIdent :: Identifier,
    functionParameters :: ParameterSection,
    functionReturnType :: PascalType,
    functionBlock :: Block
  }
  deriving (Eq, Show)

newtype ParameterSection = ParameterSection { parameterSectionParams :: [Parameter] }
  deriving (Eq, Show)

data Parameter = Parameter 
  { paramIdents :: [VariableIdent],
    paramType :: PascalType
  }
  deriving (Eq, Show)

data PascalType = PascalString | PascalInteger | PascalReal | PascalBoolean | PascalChar | PascalVoid
  deriving (Eq, Show)

data PascalTypedValue
  = PascalStringValue String
  | PascalIntegerValue Int
  | PascalRealValue Double
  | PascalBooleanValue Bool
  | PascalCharValue Char
  deriving (Eq, Show)

newtype Statements = Statements { statements :: [Statement] } 
  deriving (Eq, Show)

type SuccessfulCase = Statement

type UnsuccessfulCase = Statement

data Statement
  = AssignmentStmt Identifier Expr
  | ProcedureStmt Identifier ParamList
  | CompoundStatement Statements
  | IfStatement Expr SuccessfulCase UnsuccessfulCase
  | ForStatement Identifier Expr Increment Expr Statement
  | WhileStatement Expr Statement
  | EmptyStatement
  deriving (Eq, Show)

newtype ParamList = ParamList { paramListParams :: [Expr] }
  deriving (Eq, Show)
  
isCompound :: Statement -> Bool
isCompound stmt = case stmt of
  CompoundStatement _ -> True
  _                   -> False

isEmpty :: Statement -> Bool  
isEmpty stmt = case stmt of
  EmptyStatement -> True
  _              -> False  

data Increment = To | DownTo
  deriving (Eq, Show)

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
  | ExprFunctionDesignator String ParamList
  deriving (Eq, Show)


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
