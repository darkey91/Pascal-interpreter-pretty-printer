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

--- represents pascal programm
data Program = Program
  { programIdent :: Identifier,
    programBlock :: Block
  }
  deriving (Eq, Show)

--- represents block with statements
data Block
  = SimpleBlock Statements
  | BlockWithVar VarDeclParts Block
  | BlockWithFunc ProcedureAndFunctionDeclParts Block
  deriving (Eq, Show)

type VarDeclParts = [VarDeclPart]

--- represents part with variable declaration
newtype VarDeclPart = VarDeclPart {varDeclPartDecls :: [VariableDeclaration]}
  deriving (Eq, Show)

--represents variable(s) declaration
data VariableDeclaration = VariableDeclaration
  { variableDeclarationIdents :: [VariableIdent],
    variableDeclarationType   :: PascalType
  }
  deriving (Eq, Show)

type ProcedureAndFunctionDeclParts = [ProcedureOrFunctionDeclaration]

--- represents part with procedure or function declaration
data ProcedureOrFunctionDeclaration = ProcedureOrFunctionDeclaration
  { functionIdent      :: Identifier,
    functionParameters :: ParameterSection,
    functionReturnType :: PascalType,
    functionBlock      :: Block
  }
  deriving (Eq, Show)

--- represents formal parameter list for function or procedure
newtype ParameterSection = ParameterSection {parameterSectionParams :: [Parameter]}
  deriving (Eq, Show)

--- represents formal parameter for function or procedure
data Parameter = Parameter
  { paramIdents :: [VariableIdent],
    paramType   :: PascalType
  }
  deriving (Eq, Show)

--- Represents supported pascal types. PascalVoid is used as returned value of procedures.
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

--- represents value typed with supported pascal type. EmptyValue is used for variables wich are not defined yet.
data PascalTypedValue
  = StringValue String
  | IntegerValue Int
  | RealValue Double
  | BooleanValue Bool
  | EmptyValue
  deriving Eq

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
  deriving (Eq, Show)

type SuccessfulCase = Statement

type UnsuccessfulCase = Statement

--- represents different kinds of statements.
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
  deriving (Eq, Show)

--- represent parameter list for function or procedure call
newtype ParamList = ParamList {paramListParams :: [Expr]}
  deriving (Eq, Show)

isCompoundStmt :: Statement -> Bool
isCompoundStmt stmt = case stmt of
  CompoundStatement _ -> True
  _                   -> False

--- represents type of iterating in for loop
data Increment = To | DownTo
  deriving (Eq, Show)

type VariableIdent = String

type Identifier = String


--- represents differend kinds of expressions. Expression should return value.
data Expr
  = ExprBracketed Expr
  | ExprVal PascalTypedValue
  | ExprBinOp BinaryOperation Expr Expr
  | ExprUnOp UnaryOperation Expr
  | ExprVar String
  | ExprFunctionCall Identifier ParamList
  deriving (Eq, Show)

--- represents supported kinds of binary operations
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
  deriving Eq

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

--- represents supported kinds of unary operations
data UnaryOperation = Not | Negate
  deriving Eq

instance Show UnaryOperation where
  show Negate = "-"
  show Not    = "not "
