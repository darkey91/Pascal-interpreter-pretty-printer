module Main where

import Parser.PascalGrammar
import Parser.PascalLexer (alexScanTokens)
import Parser.PascalParser (parsePascalCode)
import System.Environment
import System.IO

    
main :: IO ()
main = do
  [fileName, mode] <- getArgs
  pascalCode <- readFile fileName
  print pascalCode




--main :: IO ()
--main = do
--  content <- getContents
--  let parseResult = (runIdentity . runExceptT . parsePascalCode) $ (alexScanTokens content)
--  putStr parseResult

--getContents >>= (\content ->
--  alexScanTokens content
--)
