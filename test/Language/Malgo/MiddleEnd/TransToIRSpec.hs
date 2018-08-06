{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.MiddleEnd.TransToIRSpec ( spec ) where

import           Language.Malgo.FrontEnd.Info
import           Language.Malgo.ID
import Language.Malgo.Type
import qualified Language.Malgo.IR.IR               as I
import qualified Language.Malgo.IR.Syntax           as S
import           Language.Malgo.MiddleEnd.TransToIR
import           Language.Malgo.Monad
import           RIO
import           Test.Hspec

spec :: Spec
spec =
  describe "trans" $ do
    t0 <- translate tuple 0
    it "Tuple" $ t0 `shouldBe` tuple'

    t1 <- translate tupleAccess 0
    it "TupleAccess" $ t1 `shouldBe` tupleAccess'

    t2 <- translate fn 1
    it "Fn" $ t2 `shouldBe` fn'
  where
    translate e i = runIO $ do
      u <- UniqSupply <$> newIORef i
      runMalgo (trans e) u (Opt "<dummy>" False False False False False False False)

x :: Info
x = Info ("<dummy>", 0, 0)

tuple :: S.Expr (ID Type)
tuple = S.Tuple x [S.Int x 0, S.Int x 1]

tuple' :: I.Expr (ID I.MType)
tuple' = I.Let (ID "$k" 0 (I.IntTy 32)) (I.Int 0)
         $ I.Let (ID "$k" 1 (I.IntTy 32)) (I.Int 1)
         $ I.Tuple[ID "$k" 0 (I.IntTy 32), ID "$k" 1 (I.IntTy 32)]

tupleAccess :: S.Expr (ID Type)
tupleAccess = S.TupleAccess x tuple 0

tupleAccess' :: I.Expr (ID I.MType)
tupleAccess' = I.Let k tuple' $ I.Access k [0, 0]
  where k = ID "$k" 2 (I.PointerTy (I.StructTy [I.IntTy 32, I.IntTy 32]))

fn :: S.Expr (ID Type)
fn = S.Fn x [(ID "x" 0 ty, ty)] (S.Var x (ID "x" 0 ty))
  where ty = "Int"

fn' :: I.Expr (ID I.MType)
fn' = I.LetRec [(fnid, Just [ID "x" 0 (I.IntTy 32)], body)] (I.Var fnid)
  where fnid = ID "$lambda" 1 (I.FunctionTy (I.IntTy 32) [I.IntTy 32])
        body = I.Var (ID "x" 0 (I.IntTy 32))
