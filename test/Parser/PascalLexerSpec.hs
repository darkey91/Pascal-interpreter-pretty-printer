module Parser.PascalLexerSpec
  ( spec,
  )
where

import Control.Exception
import Parser.PascalLexer
import Test.Hspec

spec :: Spec
spec = do
  describe "Lexer tests" $
    do
      it "empty" $ do alexScanTokens "    " `shouldBe` []
      it "key words" $ do alexScanTokens "while FOR DownTo" `shouldBe` [WhileToken, ForToken, DownToToken]
      it "sings" $ do alexScanTokens ":= . .." `shouldBe` [AssignToken, DotToken, DotDotToken]
      it "integer variable declaration" $ do
        alexScanTokens "var a: Integer;"
          `shouldBe` [VarToken, IdentifierToken "a", ColonToken, IntegerToken, SemiToken]
      it "boolean variable assign" $ do
        alexScanTokens "a := true"
          `shouldBe` [IdentifierToken "a", AssignToken, TrueValToken]
      it "parse numbers" $ do
        alexScanTokens "42 -42 42.0 -42.0"
          `shouldBe` [IntegerValToken 42, MinusToken, IntegerValToken 42, RealValToken 42.0, MinusToken, RealValToken 42.0]
      it "parse rel operators" $ do
        alexScanTokens "= >= < > <= <>"
          `shouldBe` [EQToken, GEToken, LTToken, GTToken, LEToken, NEQToken]
      it "brackets" $ do
        alexScanTokens "{ }" `shouldBe` []
        alexScanTokens "( )" `shouldBe` [LParenToken, RParenToken]
        alexScanTokens "[ ]" `shouldBe` [LBracketToken, RBracketToken]
