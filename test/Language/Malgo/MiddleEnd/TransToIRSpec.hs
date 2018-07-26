{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.MiddleEnd.TransToIRSpec ( spec ) where

import           Language.Malgo.FrontEnd.Info
import           Language.Malgo.ID
import qualified Language.Malgo.IR.IR               as I
import qualified Language.Malgo.IR.Syntax           as S
import           Language.Malgo.MiddleEnd.TransToIR
import           Language.Malgo.Monad
import           RIO
import           Test.Hspec

spec :: Spec
spec =
  describe "trans" $ do
    t0 <- translate tuple
    it "Tuple" $ t0 `shouldBe` tuple'
  where
    translate e = runIO $ do
      u <- newIORef 0
      runMalgo (trans e) (UniqSupply u)

x :: Info
x = Info ("<dummy>", 0, 0)

tuple :: S.Expr a
tuple = S.Tuple x [S.Int x 0, S.Int x 1]

tuple' :: I.Expr (ID I.MType)
tuple' = I.Let (ID "$k" 0 (I.IntTy 32)) (I.Int 0)
         $ I.Let (ID "$k" 1 (I.IntTy 32)) (I.Int 1)
         $ I.Tuple[ID "$k" 0 (I.IntTy 32), ID "$k" 1 (I.IntTy 32)]
