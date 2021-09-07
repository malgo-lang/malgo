module Koriel.Core.LambdaLift
  ( lambdalift,
  )
where

import Control.Lens (At (at), Lens', lens, traverseOf, traversed, use, (<>=), (?=))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Koriel.Core.Flat
import Koriel.Core.Syntax
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Relude.Extra.Map (member)

data LambdaLiftState = LambdaLiftState
  { _funcs :: HashMap (Id Type) ([Id Type], Exp (Id Type)),
    _knowns :: HashSet (Id Type)
  }

funcs :: Lens' LambdaLiftState (HashMap (Id Type) ([Id Type], Exp (Id Type)))
funcs = lens _funcs (\l x -> l {_funcs = x})

knowns :: Lens' LambdaLiftState (HashSet (Id Type))
knowns = lens _knowns (\l x -> l {_knowns = x})

lambdalift :: MonadIO m => UniqSupply -> Program (Id Type) -> m (Program (Id Type))
lambdalift us Program {..} =
  runReaderT ?? us $
    evalStateT ?? LambdaLiftState {_funcs = mempty, _knowns = HashSet.fromList $ map fst _topFuncs} $ do
      topFuncs <- traverse (\(f, (ps, e)) -> (f,) . (ps,) <$> llift e) _topFuncs
      funcs <>= HashMap.fromList topFuncs
      knowns <>= HashSet.fromList (map fst topFuncs)
      LambdaLiftState {_funcs} <- get
      -- TODO: lambdalift _topVars
      traverseOf appProgram (pure . flat) $ Program _moduleName _topVars (HashMap.toList _funcs)

llift :: (MonadIO f, MonadState LambdaLiftState f, MonadReader UniqSupply f) => Exp (Id Type) -> f (Exp (Id Type))
llift (Call (Var f) xs) = do
  ks <- use knowns
  if f `member` ks then pure $ CallDirect f xs else pure $ Call (Var f) xs
llift (Let [LocalDef n (Fun xs call@Call {})] e) = do
  call' <- llift call
  Let [LocalDef n (Fun xs call')] <$> llift e
llift (Let [LocalDef n o@(Fun _ ExtCall {})] e) = Let [LocalDef n o] <$> llift e
llift (Let [LocalDef n o@(Fun _ CallDirect {})] e) = Let [LocalDef n o] <$> llift e
llift (Let [LocalDef n (Fun as body)] e) = do
  backup <- get
  ks <- use knowns
  -- nがknownだと仮定してlambda liftする
  knowns . at n ?= ()
  body' <- llift body
  funcs . at n ?= (as, body')
  (e', _) <- localState $ llift e
  -- (Fun as body')の自由変数がknownsを除いてなく、e'の自由変数にnが含まれないならnはknown
  -- (Call n _)は(CallDirect n _)に変換されているので、nが値として使われているときのみ自由変数になる
  let fvs = HashSet.difference (freevars body') (ks <> HashSet.fromList as)
  if null fvs && not (n `member` freevars e')
    then llift e
    else do
      put backup
      body' <- llift body
      let fvs = HashSet.difference (freevars body') (ks <> HashSet.fromList as)
      newFun <- def (idToText n) (toList fvs <> as) body'
      Let [LocalDef n (Fun as (CallDirect newFun $ map Var $ toList fvs <> as))] <$> llift e
llift (Let ds e) = Let ds <$> llift e
llift (Match e cs) = Match <$> llift e <*> traverseOf (traversed . appCase) llift cs
llift e = pure e

def :: (MonadIO m, MonadState LambdaLiftState m, MonadReader env m, HasUniqSupply env) => Text -> [Id Type] -> Exp (Id Type) -> m (Id Type)
def name xs e = do
  f <- newInternalId ("$raw_" <> name) (map typeOf xs :-> typeOf e)
  funcs . at f ?= (xs, e)
  pure f
