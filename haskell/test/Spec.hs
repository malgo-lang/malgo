import           Test.Hspec

import qualified Language.Malgo.Syntax as S

spec = do
  describe "show :: Show AST" $ do
    it "sample1" $ do
      show S.sample1 `shouldBe` "(def ans:Int 42)"

main :: IO ()
main = hspec spec
