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

--test todo remove
programAst =
  BlockWithVar
    [ VarDeclPart
        { varDeclPartDecls =
            [ VariableDeclaration
                { variableDeclarationIdents =
                    [ "i",
                      "j",
                      "tmp",
                      "size"
                    ],
                  variableDeclarationType = PascalInteger
                },
              VariableDeclaration
                { variableDeclarationIdents =
                    [ "i",
                      "j",
                      "tmp",
                      "size"
                    ],
                  variableDeclarationType = PascalInteger
                }
            ]
        }
    ]
    ( BlockWithFunc
        [ ProcedureOrFunctionDeclaration
            { functionIdent = "wrongSum",
              functionParameters =
                ParameterSection
                  { parameterSectionParams =
                      [ Parameter
                          { paramIdents =
                              [ "a",
                                "b"
                              ],
                            paramType = PascalInteger
                          }
                      ]
                  },
              functionReturnType = PascalInteger,
              functionBlock =
                BlockWithVar
                  [ VarDeclPart
                      { varDeclPartDecls =
                          [ VariableDeclaration
                              { variableDeclarationIdents =
                                  [ "i",
                                    "j",
                                    "tmp",
                                    "size"
                                  ],
                                variableDeclarationType = PascalInteger
                              },
                            VariableDeclaration
                              { variableDeclarationIdents =
                                  [ "i",
                                    "j",
                                    "tmp",
                                    "size"
                                  ],
                                variableDeclarationType = PascalInteger
                              }
                          ]
                      }
                  ]
                  ( SimpleBlock
                      ( Statements
                          { statements =
                              [ AssignmentStmt
                                  "getSum"
                                  ( ExprPlus
                                      (ExprPlus (ExprVar "kek") (ExprVar "a"))
                                      (ExprVar "b")
                                  ),
                                EmptyStatement
                              ]
                          }
                      )
                  )
            },
          ProcedureOrFunctionDeclaration
            { functionIdent = "ReadArr",
              functionParameters = ParameterSection {parameterSectionParams = []},
              functionReturnType = PascalVoid,
              functionBlock =
                SimpleBlock
                  ( Statements
                      { statements =
                          [ ProcedureStmt
                              "readln"
                              ( ParamList
                                  { paramListParams = [ExprVar "size"]
                                  }
                              ),
                            ForStatement
                              "i"
                              (ExprVal (IntegerValue 1))
                              To
                              (ExprVar "size")
                              ( ProcedureStmt
                                  "writeLn"
                                  ( ParamList
                                      { paramListParams =
                                          [ExprVal (StringValue "'Hi'")]
                                      }
                                  )
                              ),
                            EmptyStatement
                          ]
                      }
                  )
            }
        ]
        ( SimpleBlock
            ( Statements
                { statements =
                    [ ProcedureStmt
                        "ReadArr"
                        (ParamList {paramListParams = []}),
                      ProcedureStmt
                        "Writeln"
                        ( ParamList
                            { paramListParams =
                                [ ExprVal (StringValue "'Size: '"),
                                  ExprVar "size"
                                ]
                            }
                        ),
                      ForStatement
                        "i"
                        (ExprVal (IntegerValue 1))
                        To
                        (ExprVar "size")
                        ( ForStatement
                            "j"
                            (ExprVal (IntegerValue 2))
                            To
                            (ExprVar "size")
                            ( CompoundStatement
                                ( Statements
                                    { statements =
                                        [ AssignmentStmt
                                            "tmp"
                                            ( ExprFunctionCall
                                                "wrongSum"
                                                ( ParamList
                                                    { paramListParams =
                                                        [ ExprVal (IntegerValue 42),
                                                          ExprVal (IntegerValue 16)
                                                        ]
                                                    }
                                                )
                                            ),
                                          EmptyStatement
                                        ]
                                    }
                                )
                            )
                        ),
                      ProcedureStmt
                        "Writeln"
                        ( ParamList
                            { paramListParams =
                                [ ExprVal (StringValue "'Wrong sum = '"),
                                  ExprVar "tmp"
                                ]
                            }
                        ),
                      EmptyStatement
                    ]
                }
            )
        )
    )



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
  putStrLn $ showProgram $ convert programAst
  args <- getArgs
  case args of
    ["pprint", fName] -> pprint fName
    ["run", fName] -> undefined
    ["ast", fName] -> dumpAst fName
    _ -> usageMsg
