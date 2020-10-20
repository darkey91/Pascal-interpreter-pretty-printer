module PrettyPrinter (pprintCode) where

import Control.Monad (join)
import Data.Char (toLower)
import Data.List (intercalate)
import Parser.PascalGrammar
import Utils

pprintCode :: Program -> String
pprintCode = prettyPrintStr

class PrettyPrintable p where
  prettyPrint :: p -> [String]

  pprintWithTab :: p -> [String]
  pprintWithTab = map ('\t' :) . prettyPrint

class PrettyPrintableStr p where
  prettyPrintStr :: p -> String

  prettyPrintStrWithQuotes :: p -> String
  prettyPrintStrWithQuotes = (\s -> "'" ++ s ++ "'") . prettyPrintStr

  pprintStringsWithTab :: [p] -> [String]
  pprintStringsWithTab = map (\c -> '\t' : prettyPrintStr c)

instance PrettyPrintable Program where
  prettyPrint p = do
    let programHeading = "program " ++ programIdent p ++ ";"
    let body = addSuffixToLastStr "." $ prettyPrint $ programBlock p
    programHeading : body

instance PrettyPrintableStr Program where
  prettyPrintStr prog = case prettyPrint prog of
    [] -> ""
    (p : ps) -> p ++ concatMap ('\n' :) ps

instance PrettyPrintable Block where
  prettyPrint (SimpleBlock stmts) = "begin" : pprintWithTab stmts ++ ["end"]
  prettyPrint (BlockWithConst constDefs block) = prettyPrint constDefs ++ prettyPrint block
  prettyPrint (BlockWithVar varDecl block) = prettyPrint varDecl ++ prettyPrint block
  prettyPrint (BlockWithFunc procAndFuncDecls block) = prettyPrint procAndFuncDecls ++ prettyPrint block

instance PrettyPrintable ConstantDefPart where
  prettyPrint constPart = do
    let constKw = "const"
    let (singleDef, otherDef) = case constantDefPartDefs constPart of
          [c] -> (' ' : prettyPrintStr c, [])
          cs -> ([], pprintStringsWithTab cs)
    (constKw ++ singleDef) : otherDef

instance PrettyPrintableStr Constant where
  prettyPrintStr c = do
    let sign = isConstNeg c ? "-" :? ""
    let value = (isConstVar c ? prettyPrintStrWithQuotes :? prettyPrintStr) . constValue
    constIdent c ++ " = " ++ sign ++ value c ++ ";"

instance PrettyPrintable VarDeclPart where
  prettyPrint varPart = do
    let varKw = "var"
    let (singleDef, otherDef) = case varDeclPartDecls varPart of
          [v] -> (' ' : prettyPrintStr v, [])
          vs -> ([], pprintStringsWithTab vs)
    (varKw ++ singleDef) : otherDef

instance PrettyPrintable ProcedureOrFunctionDeclaration where
  prettyPrint fDecl = do
    let params = prettyPrintStr $ functionParameters fDecl
    let (funcOrProc, returnType) = case functionReturnType fDecl of
          PascalVoid -> ("procedure ", "")
          otherType -> ("function ", ':' : ' ' : prettyPrintStr otherType)
    (funcOrProc ++ functionIdent fDecl ++ params ++ returnType ++ ";") : addSuffixToLastStr ";" (prettyPrint (functionBlock fDecl))

instance PrettyPrintable Statements where
  prettyPrint stmts = case statements stmts of
    [stmt] -> prettyPrint stmt
    st -> do
      let initPart = join $ map (addSuffixToLastStr ";" . prettyPrint) (init st)
      let lastPart = prettyPrint $ last st
      initPart ++ lastPart

pprintStatement :: Statement -> [String]
pprintStatement s = (isCompound s ? prettyPrint :? pprintWithTab) s

instance PrettyPrintable Statement where
  prettyPrint (AssignmentStmt ident expr) = [ident ++ " := " ++ prettyPrintStr expr]
  prettyPrint (ProcedureStmt ident params) = [ident ++ prettyPrintStr params]
  prettyPrint (CompoundStatement stmts) = "begin" : pprintWithTab stmts ++ ["end"]
  prettyPrint (WhileStatement expr stmt) = unwords ["while", prettyPrintStr expr, "do"] : pprintStatement stmt
  prettyPrint EmptyStatement = []
  prettyPrint (ForStatement ident exprFrom incr exprTo stmt) = do
    let forHeader = unwords ["for", ident, ":=", prettyPrintStr exprFrom, prettyPrintStr incr, prettyPrintStr exprTo, "do"]
    forHeader : pprintStatement stmt
  prettyPrint (IfStatement expr succStmt unsuccStmt) = do
    let ifHeader = "if " ++ prettyPrintStr expr ++ " then"
    let succCase = pprintStatement succStmt
    let unsuccCase = case unsuccStmt of
          EmptyStatement -> []
          other -> "else" : pprintStatement other
    ifHeader : succCase ++ succCase ++ unsuccCase

instance PrettyPrintableStr ParamList where
  prettyPrintStr p = '(' : intercalate ", " (map prettyPrintStr $ paramListParams p) ++ ")"

instance PrettyPrintableStr ParameterSection where
  prettyPrintStr p = case parameterSectionParams p of
   [] -> ""
   ps -> '(' : intercalate ", " (map prettyPrintStr ps) ++ ")"

instance PrettyPrintableStr Expr where
  prettyPrintStr (ExprBracketed expr) = '(' : prettyPrintStr expr ++ ")"
  prettyPrintStr (ExprVal t) = prettyPrintStr t
  prettyPrintStr (ExprNeg expr) = '-' : prettyPrintStr expr
  prettyPrintStr (ExprPlus l r) = prettyPrintStr l ++ " + " ++ prettyPrintStr r
  prettyPrintStr (ExprMinus l r) = prettyPrintStr l ++ " - " ++ prettyPrintStr r
  prettyPrintStr (ExprMul l r) = prettyPrintStr l ++ " * " ++ prettyPrintStr r
  prettyPrintStr (ExprDiv l r) = prettyPrintStr l ++ " \\ " ++ prettyPrintStr r
  prettyPrintStr (ExprIntDiv l r) = prettyPrintStr l ++ " div " ++ prettyPrintStr r
  prettyPrintStr (ExprEq l r) = prettyPrintStr l ++ " = " ++ prettyPrintStr r
  prettyPrintStr (ExprNeq l r) = prettyPrintStr l ++ " <> " ++ prettyPrintStr r
  prettyPrintStr (ExprGT l r) = prettyPrintStr l ++ " > " ++ prettyPrintStr r
  prettyPrintStr (ExprLT l r) = prettyPrintStr l ++ " < " ++ prettyPrintStr r
  prettyPrintStr (ExprGE l r) = prettyPrintStr l ++ " >= " ++ prettyPrintStr r
  prettyPrintStr (ExprLE l r) = prettyPrintStr l ++ " <= " ++ prettyPrintStr r
  prettyPrintStr (ExprAnd l r) = prettyPrintStr l ++ " and " ++ prettyPrintStr r
  prettyPrintStr (ExprOr l r) = prettyPrintStr l ++ " or " ++ prettyPrintStr r
  prettyPrintStr (ExprNot expr) = "not " ++ prettyPrintStr expr
  prettyPrintStr (ExprVar str) = str
  prettyPrintStr (ExprFunctionDesignator str params) = str ++ prettyPrintStr params

instance PrettyPrintableStr Parameter where
  prettyPrintStr p = do
    let idents = intercalate ", " $ paramIdents p
    let pasType = prettyPrintStr $ paramType p
    idents ++ ": " ++ pasType

instance PrettyPrintableStr VariableDeclaration where
  prettyPrintStr v = do
    let idents = intercalate ", " $ variableDeclarationIdents v
    let pasType = prettyPrintStr $ variableDeclarationType v
    idents ++ ": " ++ pasType ++ ";"
  
instance PrettyPrintableStr PascalTypedValue where
  prettyPrintStr (PascalStringValue t) = t
  prettyPrintStr (PascalIntegerValue t) = prettyPrintStr t
  prettyPrintStr (PascalRealValue t) = prettyPrintStr t
  prettyPrintStr (PascalBooleanValue t) = prettyPrintStr t
  prettyPrintStr (PascalCharValue t) = "'" ++ [t] ++ "'"

instance PrettyPrintableStr PascalType where
  prettyPrintStr PascalString = "string"
  prettyPrintStr PascalInteger = "integer"
  prettyPrintStr PascalReal = "real"
  prettyPrintStr PascalBoolean = "boolean"
  prettyPrintStr PascalChar = "char"
  prettyPrintStr PascalVoid = ""

instance PrettyPrintableStr Increment where
  prettyPrintStr To = "to"
  prettyPrintStr DownTo = "downTo"

instance (PrettyPrintable a) => PrettyPrintable [a] where
  prettyPrint = join . map prettyPrint

instance PrettyPrintableStr Char where
  prettyPrintStr c = [c]

instance PrettyPrintableStr Int where
  prettyPrintStr i = show i

instance PrettyPrintableStr Double where
  prettyPrintStr d = show d

instance PrettyPrintableStr Bool where
  prettyPrintStr b = map toLower $ show b

addSuffixToLastStr :: String -> [String] -> [String]
addSuffixToLastStr _ [] = []
addSuffixToLastStr c ss = init ss ++ [last ss ++ c]
