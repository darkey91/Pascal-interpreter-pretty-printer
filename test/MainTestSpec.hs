module MainTestSpec (spec) where

import Code
import Parser.ParseResult (ParseResult (..))
import Parser.PascalGrammar
import Parser.PascalLexer (alexScanTokens)
import Parser.PascalParser (parsePascalCode)
import Test.Hspec

program = unlines ["program Test;",
                  "var a, b, c, d: Integer;",
                  "function calculator(a, b, c, d: integer): integer;",
                  "begin",
                  "calculator :=  a + b  * c - d;",
                  "end;",
                  "begin",
                  "readln(a,b,c,d);",
                  "writeln('a + b * c - d = ', calculator(a,b,c,d));",
                  "end."]

ast = Program
    { programIdent = "Test"
    , programBlock = BlockWithVar
        [ VarDeclPart
            { varDeclPartDecls = [ VariableDeclaration
              { variableDeclarationIdents = [ "a" , "b" , "c" , "d" ], variableDeclarationType = PascalInteger } ]} ]
        ( BlockWithFunc
            [ ProcedureOrFunctionDeclaration
                { functionIdent = "calculator"
                , functionParameters = ParameterSection
                    { parameterSectionParams = [ Parameter { paramIdents = [ "a", "b", "c", "d" ], paramType = PascalInteger} ]}
                , functionReturnType = PascalInteger
                , functionBlock = SimpleBlock
                    ( Statements
                        { statements =
                            [ AssignmentStmt "calculator"
                                ( ExprBinOp Minus
                                    ( ExprBinOp Plus ( ExprVar "a" )
                                        ( ExprBinOp Mul ( ExprVar "b" ) ( ExprVar "c" ) )
                                    ) ( ExprVar "d" )
                                )
                            , EmptyStatement
                            ]
                        }
                    )
                }
            ]
            ( SimpleBlock
                ( Statements
                    { statements =
                        [ ReadlnStmt [ "a", "b", "c", "d" ]
                        , WritelnStmt ( ParamList { paramListParams =
                                    [ ExprVal (StringValue "a + b * c - d = "),
                                     ExprFunctionCall "calculator" ( ParamList { paramListParams = [ ExprVar "a", ExprVar "b", ExprVar "c", ExprVar "d" ] } )
                                    ] } )
                        , EmptyStatement
                        ]
                    }
                )
            )
        )
    }

varDecl :: Identifier -> Code
varDecl ident = liftF $ VDeclaration ident PascalInteger ()

spec :: Spec
spec = do
  describe "Parser test" $ do
    it "parse test program" $ (parsePascalCode $ alexScanTokens program ) `shouldBe` (Ok ast)
