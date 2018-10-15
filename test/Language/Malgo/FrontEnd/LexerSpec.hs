{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
module Language.Malgo.FrontEnd.LexerSpec where

import           Language.Malgo.FrontEnd.Lexer
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.Token
import           Test.Hspec
import           Universum

spec :: Spec
spec = describe "Lexer" $ do
  lextest "keywords"
    "type if then else let in rec true false forall"
    [TYPE, IF, THEN, ELSE, LET, IN, REC, TRUE, FALSE, FORALL]
  lextest "operators"
    "+ - * / % +. -. *. /. == /= < > <= >= & |"
    [ PLUS, MINUS, ASTERISK, SLASH, PERCENT, PLUS_DOT, MINUS_DOT
    , ASTERISK_DOT, SLASH_DOT, EQ_OP, NEQ_OP, LT_OP, GT_OP, LE_OP
    , GE_OP, AND_OP, OR_OP]

  lextest "annotation"
    "f : forall a. a -> a"
    [ID "f", COLON, FORALL, ID "a", DOT, ID "a", ARROW, ID "a"]
  lextest "function definition"
    "f x = x"
    [ID "f", ID "x", EQUAL, ID "x"]
  lextest "type alias definition"
    "type T = Array Int"
    [TYPE, LID "T", EQUAL, LID "Array", LID "Int"]

ss :: SrcSpan
ss = SrcSpan "<test>" 0 0 0 0

tok :: Tag -> Token
tok = Loc ss

toks :: [Tag] -> Either a [Token]
toks = Right . map tok

lextest :: String -> Text -> [Tag] -> SpecWith ()
lextest desc str ts = do
  t <- lex "<test>" str
  it desc $ t `shouldBe` toks ts
