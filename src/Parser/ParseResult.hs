module Parser.ParseResult
  ( ParseResult (..)
  , thenR
  , returnR
  , parseError
  )
where

import Parser.PascalLexer (Token (..))

data ParseResult a
  = Ok a
  | Fail String

thenR :: ParseResult a -> (a -> ParseResult b) -> ParseResult b
r `thenR` f = case r of
    Ok a   -> f a
    Fail s -> Fail s

returnR :: a -> ParseResult a
returnR = Ok

parseError :: [Token] -> ParseResult a
parseError [] = Fail "Unexpected EOF"
parseError (t:_) =  Fail $ "Parse error somewhere near line " ++ show (tokenLine t) ++ ", column " ++ show (tokenColumn t) ++ ": " ++ tokenValue t
