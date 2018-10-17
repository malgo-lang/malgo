{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
module Language.Malgo.FrontEnd.ParserSpec where

import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.Parser
import           Language.Malgo.FrontEnd.Token
import           Language.Malgo.IR.AST
import           Language.Malgo.Type
import           Test.Hspec
import           Universum                      hiding (Type)

spec :: Spec
spec = describe "Parser" $ do
  parseTest "annotation"
    [ID "f", COLON, FORALL, ID "a", DOT, ID "a", ARROW, ID "a"]
    [ScAnn ss "f" (Forall ["a"] (TyApp ArrowC [TyVar "a", TyVar "a"]))]
  parseTest "function definition"
    [ID "f", ID "x", EQUAL, ID "x"]
    [ScDef ss "f" ["x"] (Var ss "x")]

ss :: SrcSpan
ss = SrcSpan "<test>" 0 0 0 0

parseTest :: String -> [Tag] -> [Decl Text] -> SpecWith ()
parseTest desc ts ast =
  it desc $ parse (map (Loc ss) ts) `shouldBe` ast
