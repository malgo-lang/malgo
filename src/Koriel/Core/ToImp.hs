module Koriel.Core.ToImp where

import Control.Lens (traverseOf, traversed, view, _2)
import Koriel.Core.Imp
import qualified Koriel.Core.Syntax as S
import Koriel.Core.Type
import Koriel.Id
import Koriel.Lens (HasBody (body), HasPatterns (patterns))
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Pretty (Pretty (pPrint), errorDoc, ($$))

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
expToBlock (S.Match scrutinees cases) = do
  matchToBlock scrutinees cases
expToBlock (S.Error t) = eval (Error t)

matchToBlock ::
  (MonadIO m, MonadReader env m, HasUniqSupply env) =>
  -- | scrutinees
  [S.Exp (Id Type)] ->
  -- | cases in S.Match
  NonEmpty (S.Case (Id Type)) ->
  m (Block (Id Type))
matchToBlock [] _ = errorDoc "matchToBlock: empty scrutinees"
matchToBlock [scrutinee] (S.Case [S.Bind x] body :| _) = do
  Block scrutinee' <- expToBlock scrutinee
  Block body <- expToBlock body
  let scrutineeBlockResult = resultOf scrutinee'
  pure $ Block $ scrutinee' <> [Eval x (Atom (Var scrutineeBlockResult))] <> body
matchToBlock [scrutinee] cases = do
  Block scrutinee' <- expToBlock scrutinee
  result <- newInternalId "match_result" (typeOf $ S.Match [scrutinee] cases)
  cases <- traverse (uncurry caseToCase <<< view patterns &&& view body) cases
  pure $ Block $ scrutinee' <> [Match result (resultOf scrutinee') cases]
matchToBlock scrutinees cases = do
  simplified <- simplifyMatch scrutinees cases
  expToBlock simplified

resultOf :: [Stmt (Id Type)] -> Id Type
resultOf [] = error "This block has no result"
resultOf [Eval x _] = x
resultOf [Match r _ _] = r
resultOf (_ : rest) = resultOf rest

-- | convert S.Case to Imp.Case
caseToCase :: (MonadReader env m, MonadIO m, HasUniqSupply env) => [S.Pat (Id Type)] -> S.Exp (Id Type) -> m (Case (Id Type))
caseToCase [S.Unpack _ con pats] e = do
  Block e <- expToBlock e
  let eValue = resultOf e
  let bindVars =
        map ?? pats $ \case
          S.Bind x -> x
          _ -> error "invalid pattern"
  pure $ Unpack con bindVars (Block $ e <> [Break eValue])
caseToCase [S.Switch u] e = do
  Block e <- expToBlock e
  let eValue = resultOf e
  pure $ Switch (unboxedToUnboxed u) (Block $ e <> [Break eValue])
caseToCase [S.Bind x] e = do
  Block e <- expToBlock e
  let eValue = resultOf e
  pure $ Bind x (Block $ e <> [Break eValue])
caseToCase ps e = do
  errorDoc $ "caseToCase: not implemented " <> pPrint (S.Case ps e)

localDefToLocalDef :: (MonadIO m, MonadReader env m, HasUniqSupply env) => S.LocalDef (Id Type) -> m (LocalDef (Id Type))
localDefToLocalDef (S.LocalDef x o) = LocalDef x <$> objToObj o

objToObj :: (MonadIO m, MonadReader env m, HasUniqSupply env) => S.Obj (Id Type) -> m (Obj (Id Type))
objToObj (S.Fun ps e) = do
  Block e <- expToBlock e
  let eValue = resultOf e
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
  let value = resultOf block
  pure $ Block $ block <> [Return value]
