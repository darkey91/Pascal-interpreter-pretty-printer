module Main where

import Code
import Control.Monad.Except
import Parser.ParseResult (ParseResult (..))
import Parser.PascalGrammar
import Parser.PascalLexer (alexScanTokens)
import Parser.PascalParser (parsePascalCode)
import PrettyPrinter (pprintCode)
import System.Environment (getArgs, getProgName)
import System.IO
import Text.Pretty.Simple (pPrint)

import Interpreter ( run )

--test todo remove
--programAst =


usageMsg :: IO ()
usageMsg = do
  appName <- getProgName
  putStrLn $ "Incorrect usage! Usage: " ++ appName ++ " <pprint|exec|ast> <pathToFile>"

parseExpr :: String -> Either String Program
parseExpr str = do
  let tokenStream = alexScanTokens str
  case parsePascalCode tokenStream of
    Ok a -> Right a
    Fail s -> Left s

pprint :: FilePath -> IO ()
pprint fileName = do
  code <- readFile fileName
  let ast = parseExpr code
  case ast of
    Left err -> putStrLn err
    Right ast -> putStr $ pprintCode ast

dumpAst :: FilePath -> IO ()
dumpAst fname = do
  code <- readFile fname
  let tokens = alexScanTokens code
  pPrint tokens
  case parsePascalCode tokens of
    Fail s -> putStrLn s
    Ok ast -> pPrint ast

exec :: FilePath -> IO ()
exec fileName = do
  code <- readFile fileName
  let ast = parseExpr code
  case ast of
      Left err -> putStrLn err
      Right ast -> run ast

main :: IO ()
main = do
--  putStrLn $ showProgram $ convert programAst
  args <- getArgs
  case args of
    ["pprint", fName] -> pprint fName
    ["exec", fName] -> exec fName
    ["ast", fName] -> dumpAst fName
    _ -> usageMsg
