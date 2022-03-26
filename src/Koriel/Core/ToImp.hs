module Koriel.Core.ToImp where

import Control.Lens (traverseOf, traversed, _2)
import Koriel.Core.Imp
import qualified Koriel.Core.Syntax as S
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude

toImp :: (MonadIO m, MonadReader env m, HasUniqSupply env) => S.Program (Id Type) -> m (Program (Id Type))
toImp S.Program {..} = do
  _topVars <- traverseOf (traversed . _2) (expToBlock >=> ret) _topVars
  _topFuncs <- traverseOf (traversed . _2 . _2) (expToBlock >=> ret) _topFuncs
  pure Program {..}

expToBlock :: (MonadIO m, MonadReader env m, HasUniqSupply env) => S.Exp (Id Type) -> m (Block (Id Type))
expToBlock (S.Atom a) = eval (Atom $ atomToAtom a)
expToBlock (S.Call f xs) = eval (Call (atomToAtom f) (map atomToAtom xs))
expToBlock (S.CallDirect f xs) = eval (CallDirect f (map atomToAtom xs))
expToBlock (S.RawCall f t xs) = eval (RawCall f t (map atomToAtom xs))
expToBlock (S.BinOp op left right) = eval (BinOp op (atomToAtom left) (atomToAtom right))
expToBlock (S.Cast t x) = eval (Cast t $ atomToAtom x)
expToBlock (S.Let defs e) = do
  defs <- traverse localDefToLocalDef defs
  Block e <- expToBlock e
  pure $ Block $ [Let defs] <> e
expToBlock (S.Match s (S.Case (S.Bind x) e :| _)) = do
  Block s <- expToBlock s
  Block e <- expToBlock e
  let sValue = lastValue s
  pure $ Block $ s <> [Eval x (Atom (Var sValue))] <> e
expToBlock e@(S.Match s cs) = do
  Block s <- expToBlock s
  result <- newInternalId "mr" (typeOf e)
  cs <- traverse caseToCase cs
  pure $ Block $ s <> [Match result (lastValue s) cs]
expToBlock (S.Error t) = eval (Error t)

lastValue :: [Stmt (Id Type)] -> Id Type
lastValue [] = error "no value"
lastValue [Eval x _] = x
lastValue [Match r _ _] = r
lastValue (_ : rest) = lastValue rest

caseToCase :: (MonadIO m, MonadReader env m, HasUniqSupply env) => S.Case (Id Type) -> m (Case (Id Type))
caseToCase (S.Case (S.Unpack con ps) e) = do
  Block e <- expToBlock e
  let eValue = lastValue e
  let vs =
        map ?? ps $ \case
          S.Bind x -> x
          _ -> error "invalid pattern"
  pure $ Unpack con vs (Block $ e <> [Break eValue])
caseToCase (S.Case (S.Switch u) e) = do
  Block e <- expToBlock e
  let eValue = lastValue e
  pure $ Switch (unboxedToUnboxed u) (Block $ e <> [Break eValue])
caseToCase (S.Case (S.Bind x) e) = do
  Block e <- expToBlock e
  let eValue = lastValue e
  pure $ Bind x (Block $ e <> [Break eValue])

localDefToLocalDef :: (MonadIO m, MonadReader env m, HasUniqSupply env) => S.LocalDef (Id Type) -> m (LocalDef (Id Type))
localDefToLocalDef (S.LocalDef x o) = LocalDef x <$> objToObj o

objToObj :: (MonadIO m, MonadReader env m, HasUniqSupply env) => S.Obj (Id Type) -> m (Obj (Id Type))
objToObj (S.Fun ps e) = do
  Block e <- expToBlock e
  let eValue = lastValue e
  pure $ Fun ps $ Block $ e <> [Return eValue]
objToObj (S.Pack t c as) = pure $ Pack t c (map atomToAtom as)

atomToAtom :: S.Atom a -> Atom a
atomToAtom (S.Var x) = Var x
atomToAtom (S.Unboxed x) = Unboxed (unboxedToUnboxed x)

unboxedToUnboxed :: S.Unboxed -> Unboxed
unboxedToUnboxed (S.Int32 x) = Int32 x
unboxedToUnboxed (S.Int64 x) = Int64 x
unboxedToUnboxed (S.Float x) = Float x
unboxedToUnboxed (S.Double x) = Double x
unboxedToUnboxed (S.Char x) = Char x
unboxedToUnboxed (S.String x) = String x
unboxedToUnboxed (S.Bool x) = Bool x

eval :: (MonadReader env m, MonadIO m, HasUniqSupply env) => Exp (Id Type) -> m (Block (Id Type))
eval e = do
  v <- newInternalId "e" (typeOf e)
  pure $ Block [Eval v e]

ret :: Applicative f => Block (Id Type) -> f (Block (Id Type))
ret (Block block) = do
  let value = lastValue block
  pure $ Block $ block <> [Return value]
