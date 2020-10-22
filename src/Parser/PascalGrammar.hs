module Parser.PascalGrammar
  ( Program (..),
    Block (..),
    Statements (..),
    Statement (..),
    ParamList (..),
    VarDeclParts,
    VarDeclPart (..),
    VariableDeclaration (..),
    ProcedureAndFunctionDeclParts,
    ProcedureOrFunctionDeclaration (..),
    ParameterSection (..),
    Parameter (..),
    PascalType (..),
    PascalTypedValue (..),
    Increment (..),
    Expr (..),
    Identifier,
    isCompoundStmt
  )
where

data Program = Program
  { programIdent :: Identifier,
    programBlock :: Block
  }
  deriving (Eq, Show)

data Block
  = SimpleBlock Statements
  | BlockWithVar VarDeclParts Block
  | BlockWithFunc ProcedureAndFunctionDeclParts Block
  deriving (Eq, Show)

type VarDeclParts = [VarDeclPart]

newtype VarDeclPart = VarDeclPart {varDeclPartDecls :: [VariableDeclaration]}
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

newtype ParameterSection = ParameterSection {parameterSectionParams :: [Parameter]}
  deriving (Eq, Show)

data Parameter = Parameter
  { paramIdents :: [VariableIdent],
    paramType :: PascalType
  }
  deriving (Eq, Show)

instance Ord Parameter where
    a `compare` b = show a `compare` show b

data PascalType = PascalString | PascalInteger | PascalReal | PascalBoolean | PascalChar | PascalVoid
  deriving (Eq, Show)

data PascalTypedValue
  = StringValue String
  | IntegerValue Int
  | RealValue Double
  | BooleanValue Bool
  | CharValue Char
  | EmptyValue
  deriving (Eq, Show)

newtype Statements = Statements {statements :: [Statement]}
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

newtype ParamList = ParamList {paramListParams :: [Expr]}
  deriving (Eq, Show)

isCompoundStmt :: Statement -> Bool
isCompoundStmt stmt = case stmt of
  CompoundStatement _ -> True
  _ -> False
  
data Increment = To | DownTo
  deriving (Eq, Show)

type VariableIdent = String

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
  | ExprFunctionCall Identifier ParamList
  | EmptyExpr
  deriving (Eq, Show)  