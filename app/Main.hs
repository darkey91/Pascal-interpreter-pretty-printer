module Main where

import Parser.PascalGrammar
import Parser.PascalLexer (alexScanTokens)
import Parser.PascalParser (parsePascalCode)
import PrettyPrinter (pprintCode)
import Control.Monad.Except

import Text.Pretty.Simple (pPrint)

import System.Environment
import System.IO

parseExpr :: String -> Either String Program
parseExpr str = runExcept $ do
    let tokenStream = alexScanTokens str
    parsePascalCode tokenStream

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
    let ast = parsePascalCode tokens
    pPrint ast


main :: IO ()
main = do
  [fileName, mode] <- getArgs
  case mode of
          "p" -> pprint fileName
          "dump" -> dumpAst fileName
          _ -> error "Unexpected run format!\nUsage enty (run|pprint) filename.pas"

--main :: IO ()
--main = do
--  content <- getContents
--  let parseResult = (runIdentity . runExceptT . parsePascalCode) $ (alexScanTokens content)
--  putStr parseResult

--getContents >>= (\content ->
--  alexScanTokens content
--)
