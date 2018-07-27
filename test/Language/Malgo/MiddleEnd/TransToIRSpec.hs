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

    t1 <- translate tupleAccess
    it "TupleAccess" $ t1 `shouldBe` tupleAccess'
  where
    translate e = runIO $
      runMalgo (trans e) . UniqSupply =<< newIORef 0

x :: Info
x = Info ("<dummy>", 0, 0)

tuple :: S.Expr a
tuple = S.Tuple x [S.Int x 0, S.Int x 1]

tuple' :: I.Expr (ID I.MType)
tuple' = I.Let (ID "$k" 0 (I.IntTy 32)) (I.Int 0)
         $ I.Let (ID "$k" 1 (I.IntTy 32)) (I.Int 1)
         $ I.Tuple[ID "$k" 0 (I.IntTy 32), ID "$k" 1 (I.IntTy 32)]

tupleAccess :: S.Expr a
tupleAccess = S.TupleAccess x tuple 0

tupleAccess' :: I.Expr (ID I.MType)
tupleAccess' = I.Let k tuple' $ I.Access k [0, 0]
  where k = ID "$k" 2 (I.PointerTy (I.StructTy [I.IntTy 32, I.IntTy 32]))
