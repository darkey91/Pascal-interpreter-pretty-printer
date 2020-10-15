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
      it "key words" $ do alexScanTokens "while FOR DownTo" `shouldBe` [While, For, DownTo]
      it "sings" $ do alexScanTokens ":= . .." `shouldBe` [Assign, Dot, DotDot]
      it "integer variable declaration" $ do
        alexScanTokens "var a: Integer;"
          `shouldBe` [Var, Identifier "a", Colon, IntegerToken, Semi]
      it "boolean variable assign" $ do
        alexScanTokens "a := true"
          `shouldBe` [Identifier "a", Assign, TrueVal]
      it "parse numbers" $ do
        alexScanTokens "42 -42 42.0 -42.0"
          `shouldBe` [IntegerVal 42, Minus, IntegerVal 42, RealVal 42.0, Minus, RealVal 42.0]
      it "parse rel operators" $ do
        alexScanTokens "= >= < > <= <>"
          `shouldBe` [EQToken, GEToken, LTToken, GTToken, LEToken, NEQToken]
      it "brackets" $ do
        alexScanTokens "{ }" `shouldBe` []
        alexScanTokens "( )" `shouldBe` [LParen, RParen]
        alexScanTokens "[ ]" `shouldBe` [LBracket, RBracket]
