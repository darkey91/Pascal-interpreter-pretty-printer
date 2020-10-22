{-# LANGUAGE DeriveFunctor #-}

module Code where

import Control.Applicative
import Control.Monad
import Parser.PascalGrammar

data PascalCodeF next
  = PrintLn String next
  | ReadString (String -> next)
  | Assign Identifier Expr next
  | Empty next
  | While Expr Code next
  | For Identifier Expr Increment Expr Code next
  | If Expr Code Code next
  | Function Identifier ParameterSection PascalType Code next
  | Expression Expr next
  | VDeclaration Identifier PascalType next
  deriving (Functor)


--todo remove
showProgram :: Show r => Free PascalCodeF r -> String 
showProgram  (Free (PrintLn str next)) = "println " ++ str ++ "\n" ++ showProgram next
showProgram  (Free (ReadString f)) = "AAAAA" 
showProgram  (Free (Assign ident expr next)) = "assign " ++ "\n" ++ showProgram next  
showProgram  (Free (Empty next)) = "empty\n" ++ showProgram next  
showProgram  (Free (While expr code next)) = "while\n" ++ showProgram next 
showProgram  (Free (For identifier exprFrom increment exprTo code next)) = "for\n" ++ showProgram next 
showProgram  (Free (If expr succCode unsuccCode next)) = "if\n" ++ showProgram next 
showProgram  (Free (Function udentifier parameterSection pascalType code next)) = "funcDecl\n" ++ showProgram next 
showProgram  (Free (Expression expr next)) = "expr " ++ show expr ++ "\n" ++ showProgram next 
showProgram  (Free (VDeclaration identifier pascaltype next )) = "vdecl\n" ++ showProgram next  
showProgram (Pure a) = show a
   

data Free f a
  = Pure a
  | Free (f (Free f a))

type Code = Free PascalCodeF ()

liftF :: Functor f => f a -> Free f a
liftF p = Free (fmap Pure p)

class CodeConverter a where
  convert :: a -> Code

instance CodeConverter Block where
  convert (SimpleBlock stmts) = convert $ statements stmts
  convert (BlockWithVar varDecl block) =
    do
      convert $ join $ map varDeclPartDecls varDecl
      convert block
  convert (BlockWithFunc funcPart block) =
    do
      convert funcPart
      convert block

instance CodeConverter VariableDeclaration where
  convert vDecl = mapM_ (\ident -> liftF $ VDeclaration ident vType ()) idents
    where
      idents = variableDeclarationIdents vDecl
      vType = variableDeclarationType vDecl

instance CodeConverter ProcedureOrFunctionDeclaration where
  convert fDecl = liftF $ Function ident paramList rType (convert block) ()
    where
      ident = functionIdent fDecl
      paramList = functionParameters fDecl
      rType = functionReturnType fDecl
      block = functionBlock fDecl

instance CodeConverter Statement where
  convert (AssignmentStmt ident expr) = liftF $ Assign ident expr ()
  convert (ProcedureStmt ident paramList) = liftF $ Expression (ExprFunctionCall ident paramList) ()
  convert (CompoundStatement stmts) = convert $ statements stmts
  convert (IfStatement expr succStmt unsuccStmt) = liftF $ If expr (convert succStmt) (convert unsuccStmt) ()
  convert (ForStatement ident exprFrom increment exprTo stmt) = liftF $ For ident exprFrom increment exprTo (convert stmt) ()
  convert (WhileStatement expr stmt) = liftF $ While expr (convert stmt) ()
  convert EmptyStatement = liftF $ Empty ()

instance CodeConverter a => CodeConverter [a] where
  convert [] = liftF $ Empty ()
  convert as = mapM_ convert as

instance Functor f => Functor (Free f) where
  f `fmap` (Pure a) = Pure $ f a
  f `fmap` (Free fs) = Free $ fmap f <$> fs

instance Functor f => Applicative (Free f) where
  pure = Pure
  (Pure f) <*> (Pure a) = Pure $ f a
  (Pure f) <*> (Free as) = Free $ fmap f <$> as
  (Free fs) <*> a = Free $ fmap (<*> a) fs

instance Functor f => Monad (Free f) where
  return = pure
  (Pure a) >>= f = f a
  (Free fs) >>= f = Free $ (>>= f) <$> fs 