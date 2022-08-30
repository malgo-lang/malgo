{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Koriel.Core.CodeGen.Scheme where

import Control.Lens (makeFieldsNoPrefix, view)
import Control.Monad.State.Lazy qualified as Lazy
import Data.String.Conversions (ConvertibleStrings (convertString))
import Koriel.Core.Op (Op (..))
import Koriel.Core.Syntax
import Koriel.Core.Type qualified as C
import Koriel.Id (Id, ModuleName, idToText)
import Koriel.Lens (HasUniqSupply (..))
import Koriel.MonadUniq (UniqSupply)
import Koriel.Prelude
import Koriel.Pretty (Pretty (..), render, (<+>))
import Koriel.Pretty qualified as P
import Malgo.Prelude (HasDstName (dstName))

data CGState = CGState

instance Semigroup CGState where
  _ <> _ = CGState

instance Monoid CGState where
  mempty = CGState

data CGEnv = CGEnv
  { _uniqSupply :: UniqSupply,
    _dstName :: FilePath
  }

makeFieldsNoPrefix ''CGEnv

runCodeGenT :: Monad m => Lazy.StateT CGState (ReaderT CGEnv m) a -> CGEnv -> m a
runCodeGenT m env = runReaderT (Lazy.evalStateT m mempty) env

codegen :: MonadIO m => FilePath -> FilePath -> UniqSupply -> ModuleName -> Program (Id C.Type) -> m ()
codegen srcPath dstPath uniqSupply modName Program {..} =
  runCodeGenT ?? CGEnv {_uniqSupply = uniqSupply, _dstName = dstPath} $ do
    traverse_ genExtFunc _extFuncs
    traverse_ genTopVar _topVars
    traverse_ genTopFunc _topFuncs

genTopFunc :: (Id C.Type, ([Id C.Type], Exp (Id C.Type))) -> m ()
genTopFunc = undefined

genExtFunc :: MonadIO m => (Text, C.Type) -> m ()
genExtFunc (_, _) = pure ()

genTopVar :: (MonadIO m, MonadReader env m, HasDstName env FilePath) => (Id C.Type, Exp (Id C.Type)) -> m ()
genTopVar (name, value) = do
  dstPath <- view dstName
  value <- genExp value
  appendFile dstPath $ render $ P.parens $ pPrint name <+> value

genExp :: MonadIO m => Exp (Id C.Type) -> m P.Doc
genExp (Atom (Var x)) = pure $ P.text $ convertString $ idToText x
genExp (Atom (Unboxed unboxed)) = genUnboxed unboxed
genExp (Call f xs) = do
  f <- genAtom f
  xs <- traverse genAtom xs
  pure $ P.parens (P.hsep $ f : xs)
genExp (CallDirect f xs) = do
  let f' = P.text $ convertString $ idToText f
  xs <- traverse genAtom xs
  pure $ P.parens (P.hsep $ f' : xs)
genExp (RawCall f _ xs) = do
  xs <- traverse genAtom xs
  pure $ P.parens (P.hsep $ P.text (convertString f) : xs)
genExp (BinOp op x y) = do
  x <- genAtom x
  y <- genAtom y
  genOp op x y

genOp :: MonadIO m => Op -> P.Doc -> P.Doc -> m P.Doc
genOp Add = \x y -> pure $ P.parens $ P.hsep ["+", x, y]
genOp Sub = \x y -> pure $ P.parens $ P.hsep ["-", x, y]
genOp Mul = \x y -> pure $ P.parens $ P.hsep ["*", x, y]
genOp Div = \x y -> pure $ P.parens $ P.hsep ["quotient", x, y]
genOp Mod = \x y -> pure $ P.parens $ P.hsep ["modulo", x, y]
genOp FAdd = \x y -> pure $ P.parens $ P.hsep ["+", x, y]
genOp FSub = \x y -> pure $ P.parens $ P.hsep ["-", x, y]
genOp FMul = \x y -> pure $ P.parens $ P.hsep ["*", x, y]
genOp FDiv = \x y -> pure $ P.parens $ P.hsep ["/", x, y]
genOp Eq = \x y -> pure $ P.parens $ P.hsep ["equal?", x, y]
genOp Neq = \x y -> pure $ P.parens $ P.hsep ["not", P.parens $ P.hsep ["equal?", x, y]]

genAtom :: MonadIO f => Atom (Id a) -> f P.Doc
genAtom (Var x) = pure $ P.text $ convertString $ idToText x
genAtom (Unboxed unboxed) = genUnboxed unboxed

genUnboxed :: MonadIO m => Unboxed -> m P.Doc
genUnboxed (Int32 i) = pure $ pPrint i
genUnboxed (Int64 i) = pure $ pPrint i
genUnboxed (Float f) = pure $ pPrint f
genUnboxed (Double f) = pure $ pPrint f
genUnboxed (Char c) = pure $ "#\\" <> P.char c
genUnboxed (String s) = pure $ pPrint s
genUnboxed (Bool True) = pure $ "#t"
genUnboxed (Bool False) = pure $ "#f"