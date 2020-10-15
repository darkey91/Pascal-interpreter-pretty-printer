{-# LANGUAGE GADTs #-}

module Parser.PascalGrammar
  ( Expr (..),
  )
where


--todo think about concatenation
data Expr a where
  ExprBracketed :: Expr a -> Expr a
  ExprNumVal :: (Num a) => a -> Expr a
  ExprNeg :: (Num a) => Expr a -> Expr a
  ExprPlus :: (Num a) => Expr a -> Expr a -> Expr a
  ExprMinus :: (Num a) => Expr a -> Expr a -> Expr a
  ExprMul :: (Num a) => Expr a -> Expr a -> Expr a
  ExprDiv :: (Fractional a) => Expr a -> Expr a -> Expr a
  ExprIntDiv :: (Integral a) => Expr a -> Expr a -> Expr a
  ExprAnd :: Expr Bool -> Expr Bool -> Expr Bool
  ExprOr ::  Expr Bool -> Expr Bool -> Expr Bool    
  ExprNot :: Expr Bool -> Expr Bool  