module Parser.PascalLexerSpec(spec) where

import Parser.PascalLexer
import Test.Hspec

spec :: Spec
spec = do
  describe "Lexer tests" $
    do
      it "empty" $ do alexScanTokens "    " `shouldBe` []
      it "key words" $ do map tokenType (alexScanTokens "while FOR DownTo") `shouldBe` [WhileToken, ForToken, DownToToken]
      it "sings" $ do map tokenType (alexScanTokens ":= . ..") `shouldBe` [AssignToken, DotToken, DotDotToken]
      it "integer variable declaration" $ do
        getTypeAndVal <$> alexScanTokens "var a: Integer;"
          `shouldBe` [(VarToken, "var"),(IdentifierToken, "a"), (ColonToken, ":"), (IntegerToken, "Integer"), (SemiToken, ";") ]
      it "boolean variable assign" $ do
        getTypeAndVal <$> alexScanTokens "a := true"
          `shouldBe` [ (IdentifierToken, "a"), (AssignToken, ":="), (TrueValToken, "true") ]
      it "parse numbers" $ do
        getTypeAndVal <$> alexScanTokens "42 -42 42.0 -42.0"
          `shouldBe` [(IntegerValToken, "42"), (MinusToken, "-"), (IntegerValToken, "42"), (RealValToken, "42.0"), (MinusToken, "-"), (RealValToken, "42.0")]
      it "parse rel operators" $ do
        getTypeAndVal <$> alexScanTokens "= >= < > <= <>"
          `shouldBe` [(EQToken, "="), (GEToken, ">="), (LTToken, "<" ), (GTToken, ">"), (LEToken, "<="), (NEQToken, "<>")]
      it "brackets" $ do
        alexScanTokens "{ }" `shouldBe` []
        getTypeAndVal <$> alexScanTokens "( )" `shouldBe` [(LParenToken, "("), (RParenToken, ")")]
        getTypeAndVal <$> alexScanTokens "[ ]" `shouldBe` [(LBracketToken, "["), (RBracketToken, "]")]

getTypeAndVal :: Token -> (TokenType, String)
getTypeAndVal t = (tokenType t, tokenValue t)
