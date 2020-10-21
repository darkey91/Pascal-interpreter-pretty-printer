module Main where

import Parser.PascalGrammar
import Parser.PascalLexer (alexScanTokens)
import Parser.PascalParser (parsePascalCode)
import Parser.ParseResult ( ParseResult(..) )
import PrettyPrinter ( pprintCode )
import Control.Monad.Except

import Text.Pretty.Simple (pPrint)

import System.Environment ( getProgName, getArgs )
import System.IO


usageMsg :: IO ()
usageMsg = do
  appName <- getProgName
  putStrLn $ "Incorrect usage! Usage: " ++ appName ++ " <pprint|run|ast> <pathToFile>"

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


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pprint", fName] -> pprint fName
    ["run", fName] -> undefined
    ["ast", fName] -> dumpAst fName
    _ -> usageMsg
