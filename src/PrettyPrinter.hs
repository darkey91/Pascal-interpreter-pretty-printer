module PrettyPrinter where

import Data.Char (toLower)
import Data.List (intercalate)
import Parser.PascalGrammar
import Utils

class PrettyPrinterable p where
  prettyPrint :: p -> String

  prettyPrintMapped :: (String -> String) -> p -> String
  prettyPrintMapped f = f . prettyPrint

  prettyPrintWithSemi :: p -> String
  prettyPrintWithSemi = prettyPrintMapped (++ ";")

  prettyPrintWithQuotes :: p -> String
  prettyPrintWithQuotes = prettyPrintMapped (\s -> "'" ++ s ++ "'")

  prettyPrintWithTab :: p -> String
  prettyPrintWithTab = prettyPrintMapped (\s -> '\n' : '\t' : s)
  
  prettyPrintElemsWithTab :: [p] -> String
  prettyPrintElemsWithTab = concatMap prettyPrintWithTab

instance PrettyPrinterable Program where
  prettyPrint p = concat ["program ", programIdent p, ";", prettyPrint $ programBlock p, "."]

instance PrettyPrinterable Block where
  prettyPrint (SimpleBlock stmts) = concat ["\nbegin", prettyPrintElemsWithTab stmts, "\nend"]
  prettyPrint (BlockWithConst constDefs block) = prettyPrint constDefs ++ prettyPrint block
  prettyPrint (BlockWithVar varDecl block) = prettyPrint varDecl ++ prettyPrint block
  prettyPrint (BlockWithFunc procAndFuncDecls block) = prettyPrint procAndFuncDecls ++ prettyPrint block

instance PrettyPrinterable ConstantDefPart where
  prettyPrint constPart = "\nconst" ++ constDefs
    where
      constDefs = case constantDefPartDefs constPart of
        [] -> ""
        [c] -> ' ' : prettyPrint c
        cs -> prettyPrintElemsWithTab cs

instance PrettyPrinterable Constant where
  prettyPrint c = unwords [ident, "=", val] ++ ";"
    where
      ident = prettyPrint (constIdent c)
      sign = isConstNeg c ? "-" :? ""
      val = sign ++ (isConstVar c ? prettyPrintWithQuotes ?: prettyPrint) c

instance PrettyPrinterable VarDeclPart where
  prettyPrint varDeclPart = "\nvar" ++ varDecls
    where
      varDecls = case varDeclPartDecls varDeclPart of
        [] -> ""
        [v] -> ' ' : prettyPrint v
        vs -> prettyPrintElemsWithTab vs

instance PrettyPrinterable VariableDeclaration where
  prettyPrint c = unwords [ident ++ ":", pasType ++ ";"]
    where
      ident = intercalate ", " $ map prettyPrint $ variableDeclarationIdents c
      pasType = prettyPrint $ variableDeclarationType c

instance PrettyPrinterable ProcedureOrFunctionDeclaration where
  prettyPrint decl = concat [funcOrProc, ident, params, returnType, prettyPrint $ functionBlock decl, ";"]
    where
      ident = prettyPrint ident
      params = prettyPrint params
      (funcOrProc, returnType) = case functionReturnType decl of
        PascalVoid -> ("\nprocedure ", "")
        otherType -> ("\nfunction", ':' : ' ' : prettyPrint otherType)

instance

printStatement :: Statement
pprintStatement s = isCompound s ? prettyPrint s ++ ";" :? prettyPrintWithTab s  

instance PrettyPrinterable Statement where
  prettyPrint (AssignmentStmt ident expr) = unwords ['\n' : prettyPrint ident, ":=", prettyPrint expr, ";"]
  prettyPrint (ProcedureStmt ident params) = concat ['\n' : ident, prettyPrint params, ";"]
  prettyPrint (CompoundStatement stmts) = concat ["\nbegin", prettyPrintElemsWithTab stmts, "\nend"]
  prettyPrint (IfStatement expr succStmt unsuccStmt) =
    concat ["\nif ", prettyPrint expr, " then", pprintSucc succStmt, unsucc, semi]
      where
        pprintSucc   = isCompound succStmt ? prettyPrint :? prettyPrintWithTab
        pprintUnsucc = isEmpty unsuccStmt ? prettyPrint :? isCompound unsuccStmt ? "\nelse" prettyPrint :? prettyPrintWithTab 
        semi = isEmpty unsuccStmt ? (isCompound succStmt ? ";" :? "") :? (isCompound unsuccStmt ? ";" :? "")
  prettyPrint (ForStatement indent exprFrom incr exprTo stmt) = 
    unwords ["\nfor", prettyPrint ident, ":=", prettyPrint exprFrom, prettyPrint inc, prettyPrint exprTo, "do", pprintStatement stmt]      
  prettyPrint (WhileStatement expr stmt) = "\nwhile " ++ prettyPrint expr ++ pprintStatement stmt
  prettyPrint (RepeatStatement stmts expr) = "\nrepeat"
  prettyPrint EmptyStatement = ""


instance PrettyPrinterable PascalTypedValue where
  prettyPrint (PascalStringValue t) = prettyPrint t
  prettyPrint (PascalIntegerValue t) = prettyPrint t
  prettyPrint (PascalRealValue t) = prettyPrint t
  prettyPrint (PascalBooleanValue t) = prettyPrint t
  prettyPrint (PascalCharValue t) = "'" ++ prettyPrint t ++ "'"

instance PrettyPrinterable PascalType where
  prettyPrint PascalString = "string"
  prettyPrint PascalInteger = "integer"
  prettyPrint PascalReal = "real"
  prettyPrint PascalBoolean = "boolean"
  prettyPrint PascalChar = "char"
  prettyPrint PascalVoid = ""

instance PrettyPrinterable Increment where
  prettyPrint To = "to"
  prettyPrint DownTo = "downTo"

instance (PrettyPrinterable a) => PrettyPrinterable [a] where
  prettyPrint = concatMap prettyPrint

instance PrettyPrinterable Char where
  prettyPrint c = [c]

instance PrettyPrinterable Int where
  prettyPrint = show

instance PrettyPrinterable Double where
  prettyPrint = show

instance PrettyPrinterable Bool where
  prettyPrint = map toLower . show
