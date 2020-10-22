{-# LANGUAGE DeriveFunctor #-}

module Code where

import Control.Applicative
import Control.Monad
import Parser.PascalGrammar

data PascalCodeF next
  = PrintLn String next
  | ReadString (String -> next)
  | AssignVar Identifier Expr next
  | Empty next
  | While Expr Statement next
  | For Identifier Expr Increment Expr Statement next
  | If Expr Statement Statement next
  | Function Identifier ParamList next
  | Expression Expr next
  deriving (Functor)

data Free f a
  = Pure a
  | Free (f (Free f a))

type Code = Free PascalCodeF ()

liftF :: Functor f => f a -> Free f a
liftF p = Free (fmap Pure p)

class CodeConverter a where
  convert :: a -> Code

--instance CodeConverter Block where
  --convert (SimpleBlock statements)
  --convert (BlockWithVar varDecl block)
  --convert (BlockWithFunc funcPart block)
--  
  --SimpleBlock Statements
  --BlockWithConst ConstantDefParts Block
  --BlockWithVar VarDeclParts Block
  --BlockWithFunc ProcedureAndFunctionDeclParts Block
--  
    

instance CodeConverter Statement where
  convert (AssignmentStmt ident expr) = liftF $ AssignVar ident expr ()
  convert (ProcedureStmt ident paramList) = liftF $ Expression (ExprFunctionCall ident paramList) ()
  convert (CompoundStatement stmts) = convert $ statements stmts
  convert (IfStatement expr succStmt unsuccStmt) = liftF $ If expr succStmt unsuccStmt ()
  convert (ForStatement ident exprFrom increment exprTo stmt) = liftF $ For ident exprFrom increment exprTo stmt ()
  convert (WhileStatement expr stmt) = liftF $ While expr stmt ()
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
