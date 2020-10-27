module Parser.PascalGrammar
  ( Program (..)
  , Block (..)
  , Statements (..)
  , Statement (..)
  , ParamList (..)
  , VarDeclParts
  , VarDeclPart (..)
  , VariableDeclaration (..)
  , ProcedureAndFunctionDeclParts
  , ProcedureOrFunctionDeclaration (..)
  , ParameterSection (..)
  , Parameter (..)
  , PascalType (..)
  , PascalTypedValue (..)
  , Increment (..)
  , Expr (..)
  , Identifier
  , BinaryOperation (..)
  , UnaryOperation (..)
  , isCompoundStmt
  , getType
  )
where

import Data.Char (toLower)

data Program = Program
  { programIdent :: Identifier,
    programBlock :: Block
  }
  deriving (Show)

data Block
  = SimpleBlock Statements
  | BlockWithVar VarDeclParts Block
  | BlockWithFunc ProcedureAndFunctionDeclParts Block
  deriving (Show)

type VarDeclParts = [VarDeclPart]

newtype VarDeclPart = VarDeclPart {varDeclPartDecls :: [VariableDeclaration]}
  deriving (Show)

data VariableDeclaration = VariableDeclaration
  { variableDeclarationIdents :: [VariableIdent],
    variableDeclarationType   :: PascalType
  }
  deriving (Show)

type ProcedureAndFunctionDeclParts = [ProcedureOrFunctionDeclaration]

data ProcedureOrFunctionDeclaration = ProcedureOrFunctionDeclaration
  { functionIdent      :: Identifier,
    functionParameters :: ParameterSection,
    functionReturnType :: PascalType,
    functionBlock      :: Block
  }
  deriving (Show)

newtype ParameterSection = ParameterSection {parameterSectionParams :: [Parameter]}
  deriving (Show)

data Parameter = Parameter
  { paramIdents :: [VariableIdent],
    paramType   :: PascalType
  }
  deriving (Show)

data PascalType
  = PascalString
  | PascalInteger
  | PascalReal
  | PascalBoolean
  | PascalVoid
  deriving Eq

instance Show PascalType where
  show PascalString  = "string"
  show PascalInteger = "integer"
  show PascalReal    = "real"
  show PascalBoolean = "boolean"
  show PascalVoid    = "void"

instance Ord PascalType where
  a `compare` b = show a `compare` show b

data PascalTypedValue
  = StringValue String
  | IntegerValue Int
  | RealValue Double
  | BooleanValue Bool
  | EmptyValue

getType :: PascalTypedValue -> PascalType
getType (StringValue _)  = PascalString
getType (IntegerValue _) = PascalInteger
getType (RealValue _)    = PascalReal
getType (BooleanValue _) = PascalBoolean
getType EmptyValue       = PascalVoid

instance Show PascalTypedValue where
  show (StringValue v)  = v
  show (IntegerValue v) = show v
  show (RealValue v)    = show v
  show (BooleanValue v) = map toLower $ show v
  show EmptyValue       = "_|_"

newtype Statements = Statements {statements :: [Statement]}
  deriving (Show)

type SuccessfulCase = Statement

type UnsuccessfulCase = Statement

data Statement
  = AssignmentStmt Identifier Expr
  | ProcedureStmt Identifier ParamList
  | CompoundStatement Statements
  | IfStatement Expr SuccessfulCase UnsuccessfulCase
  | ForStatement Identifier Expr Increment Expr Statement
  | WhileStatement Expr Statement
  | WritelnStmt ParamList
  | ReadlnStmt [VariableIdent]
  | EmptyStatement
  deriving (Show)

newtype ParamList = ParamList {paramListParams :: [Expr]}
  deriving (Show)

isCompoundStmt :: Statement -> Bool
isCompoundStmt stmt = case stmt of
  CompoundStatement _ -> True
  _                   -> False

data Increment = To | DownTo
  deriving (Show)

type VariableIdent = String

type Identifier = String

data Expr
  = ExprBracketed Expr
  | ExprVal PascalTypedValue
  | ExprBinOp BinaryOperation Expr Expr
  | ExprUnOp UnaryOperation Expr
  | ExprVar String
  | ExprFunctionCall Identifier ParamList
  deriving Show

data BinaryOperation
  = Plus
  | Minus
  | Mul
  | Div
  | IntDiv
  | EqOp
  | NeqOp
  | GTOp
  | LTOp
  | GEOp
  | LEOp
  | And
  | Or

instance Show BinaryOperation where
  show Plus   = " + "
  show Minus  = " - "
  show Mul    = " * "
  show Div    = " \\ "
  show IntDiv = " div "
  show EqOp   = " = "
  show NeqOp  = " <> "
  show GTOp   = " > "
  show LTOp   = " < "
  show GEOp   = " >= "
  show LEOp   = " <= "
  show And    = " and "
  show Or     = " or "

data UnaryOperation = Not | Negate

instance Show UnaryOperation where
  show Negate = "-"
  show Not    = "not "
