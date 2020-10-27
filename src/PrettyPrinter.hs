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

  pprintStringsWithTab :: [p] -> [String]
  pprintStringsWithTab = map (\c -> '\t' : prettyPrintStr c)

instance PrettyPrintable Program where
  prettyPrint p = do
    let programHeading = "program " ++ programIdent p ++ ";"
    let body = addSuffixToLastStr "." $ prettyPrint $ programBlock p
    programHeading : body

instance PrettyPrintableStr Program where
  prettyPrintStr prog = case prettyPrint prog of
    []       -> ""
    (p : ps) -> p ++ concatMap ('\n' :) ps

instance PrettyPrintable Block where
  prettyPrint (SimpleBlock stmts) = "begin" : pprintWithTab stmts ++ ["end"]
  prettyPrint (BlockWithVar varDecl block) = prettyPrint varDecl ++ prettyPrint block
  prettyPrint (BlockWithFunc procAndFuncDecls block) = prettyPrint procAndFuncDecls ++ prettyPrint block

instance PrettyPrintable VarDeclPart where
  prettyPrint varPart = do
    let varKw = "var"
    let (singleDef, otherDef) = case varDeclPartDecls varPart of
          [v] -> (' ' : prettyPrintStr v, [])
          vs  -> ([], pprintStringsWithTab vs)
    (varKw ++ singleDef) : otherDef

instance PrettyPrintable ProcedureOrFunctionDeclaration where
  prettyPrint fDecl = do
    let params = prettyPrintStr $ functionParameters fDecl
    let (funcOrProc, returnType) = case functionReturnType fDecl of
          PascalVoid -> ("\nprocedure ", "")
          otherType  -> ("\nfunction ", ':' : ' ' : prettyPrintStr otherType)
    (funcOrProc ++ functionIdent fDecl ++ params ++ returnType ++ ";") : addSuffixToLastStr ";" (prettyPrint (functionBlock fDecl))

instance PrettyPrintable Statements where
  prettyPrint stmts = case statements stmts of
    [stmt] -> prettyPrint stmt
    st -> do
      let initPart = join $ map (addSuffixToLastStr ";" . prettyPrint) (init st)
      let lastPart = prettyPrint $ last st
      initPart ++ lastPart

pprintStatement :: Statement -> [String]
pprintStatement s = (isCompoundStmt s ? prettyPrint :? pprintWithTab) s

instance PrettyPrintable Statement where
  prettyPrint (AssignmentStmt ident expr) = [ident ++ " := " ++ prettyPrintStr expr]
  prettyPrint (ProcedureStmt ident params) = [ident ++ prettyPrintStr params]
  prettyPrint (WritelnStmt params) = ["writeln" ++ prettyPrintStr params]
  prettyPrint (ReadlnStmt idents) = ["readln(" ++ intercalate ", " idents ++ ")"]
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
          other          -> "else" : pprintStatement other
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
  prettyPrintStr (ExprBinOp binOp l r) = prettyPrintStr l ++ prettyPrintStr binOp ++ prettyPrintStr r
  prettyPrintStr (ExprUnOp unOp expr) = prettyPrintStr unOp ++ prettyPrintStr expr
  prettyPrintStr (ExprVar str) = str
  prettyPrintStr (ExprFunctionCall str params) = str ++ prettyPrintStr params

instance PrettyPrintableStr BinaryOperation where
  prettyPrintStr = show

instance PrettyPrintableStr UnaryOperation where
  prettyPrintStr = show

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
  prettyPrintStr EmptyValue = ""
  prettyPrintStr t          = show t

instance PrettyPrintableStr PascalType where
  prettyPrintStr PascalVoid = ""
  prettyPrintStr other      = show other

instance PrettyPrintableStr Increment where
  prettyPrintStr To     = "to"
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
