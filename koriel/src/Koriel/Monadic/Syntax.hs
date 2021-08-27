{-# LANGUAGE UndecidableInstances #-}

module Koriel.Monadic.Syntax where

import Data.Monoid
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude hiding (Fold, exp)
import Koriel.Pretty
import qualified RIO.HashSet as HashSet
import qualified Text.PrettyPrint as Pretty

data Program = Program {topVars :: [(Var, Exp)], topFuncs :: [(Var, Func)], externals :: [(Var, String)]}
  deriving stock (Show, Eq, Ord, Generic)

instance Pretty Program where
  pPrint Program {..} =
    "variables:"
      $$ nest 2 (pPrintDecls topVars)
      $$ "functions:"
      $$ nest 2 (pPrintDecls topFuncs)
      $$ "externals:"
      $$ nest 2 (pPrintDecls externals)
    where
      pPrintDecls nvs = sep $ punctuate ";" $ map (uncurry pPrintDecl) nvs
      pPrintDecl name value =
        pPrint name <+> ":" <+> pPrint (view idMeta name)
          $$ pPrint name <+> "=" <+> pPrint value

definedVars :: Program -> HashSet Var
definedVars Program {..} = HashSet.fromList (map fst topVars) <> HashSet.fromList (map fst topFuncs) <> HashSet.fromList (map fst externals)

data Func = Func [TyVar] [Var] Exp
  deriving stock (Show, Eq, Ord, Generic)

instance Pretty Func where
  pPrint (Func ts ps e) =
    sep
      [ "Λ" <+> sep (punctuate "," $ map pPrint ts) <> ".",
        "λ" <+> sep (punctuate "," $ map pPrint ps) <> ".",
        nest 2 $ pPrint e
      ]

data ApplyMode
  = -- | closure
    Indirect
  | -- | known function
    Direct
  deriving stock (Show, Eq, Ord, Generic)

data Exp
  = Bind Exp Var Exp
  | Unit Value
  | Match Value [(Value, Exp)]
  | Apply ApplyMode Var [Value]
  | Alloc Value
  | Fetch Var (Maybe (Tag, Int))
  | Update Var Value
  | Closure Func
  | -- type operator
    TyAbs [TyVar] Exp
  | TyApply Var [Type]
  | Fold Type Value
  | Unfold Type Value
  | -- static exception
    Exit Type -- go to next handler
  | Catch Exp Exp -- try the first expression and catch `exit` with the second expression
  deriving stock (Show, Eq, Ord, Generic)

instance Pretty Exp where
  pPrint (Bind e1 x e2) = pPrint e1 <> ";" <+> pPrint x <+> pPrint (view idMeta x) <+> "->" $$ pPrint e2
  pPrint (Unit x) = "unit" <+> pPrint x
  pPrint (Match x cs) = "match" <+> pPrint x <+> "{" $$ nest 2 (sep $ punctuate ";" $ map pPrintClause cs) $$ "}"
    where
      pPrintClause (v, e) = pPrint v <+> "->" <+> pPrint e
  pPrint (Apply Indirect f xs) = "apply" <+> sep (pPrint f : map pPrint xs)
  pPrint (Apply Direct f xs) = "apply*" <+> sep (pPrint f : map pPrint xs)
  pPrint (Alloc v) = "alloc" <+> pPrint v
  pPrint (Fetch c Nothing) = "fetch" <+> pPrint c
  pPrint (Fetch c (Just n)) = "fetch" <+> pPrint c <+> "[" <> pPrint n <> "]"
  pPrint (Update var value) = "update" <+> pPrint var <+> pPrint value
  pPrint (Closure func) = "closure" <+> parens (pPrint func)
  pPrint (TyAbs ts e) = "Λ" <+> sep (punctuate "," $ map pPrint ts) <> "." <+> pPrint e
  pPrint (TyApply x vs) = "tyapply" <+> sep (pPrint x : map pPrint vs)
  pPrint (Fold t x) = "fold" <+> pPrint t <+> pPrint x
  pPrint (Unfold t x) = "unfold" <+> pPrint t <+> pPrint x
  pPrint (Exit t) = "exit" <+> pPrint t
  pPrint (Catch e1 e2) = "catch" <+> pPrint e1 <+> "with" $$ nest 2 (pPrint e2)

freevars :: Exp -> HashSet Var
freevars (Bind e1 x e2) = freevars e1 <> HashSet.delete x (freevars e2)
freevars (Unit v) = freevars' v
freevars (Match v cs) = freevars' v <> mconcat (map (\(pat, expr) -> HashSet.difference (freevars expr) (freevars' pat)) cs)
freevars (Apply _ f xs) = HashSet.insert f $ mconcat $ map freevars' xs
freevars (Alloc v) = freevars' v
freevars (Fetch v _) = HashSet.singleton v
freevars (Update var value) = HashSet.insert var $ freevars' value
freevars (Closure (Func _ ps e)) = HashSet.difference (freevars e) (HashSet.fromList ps)
freevars (TyAbs _ e) = freevars e
freevars (TyApply v _) = HashSet.singleton v
freevars (Fold _ v) = freevars' v
freevars (Unfold _ v) = freevars' v
freevars (Exit _) = HashSet.empty
freevars (Catch e1 e2) = freevars e1 <> freevars e2

freevars' :: Value -> HashSet Var
freevars' (Pack _ _ values) = mconcat $ map freevars' values
freevars' (Var var) = HashSet.singleton var
freevars' _ = HashSet.empty

data Value
  = Pack Type Tag [Value]
  | Int32 Int32
  | Int64 Int64
  | Float Float
  | Double Double
  | Char Char
  | String String
  | Var Var
  deriving stock (Show, Eq, Ord, Generic)

instance Pretty Value where
  pPrint (Pack _ tag vs) = "<" <> hsep (punctuate "," $ pPrint tag : map pPrint vs) <> ">"
  pPrint (Int32 x) = Pretty.int (fromIntegral x)
  pPrint (Int64 x) = Pretty.int (fromIntegral x) <> "L"
  pPrint (Float x) = pPrint x <> "F"
  pPrint (Double x) = pPrint x
  pPrint (Char x) = Pretty.quotes $ pPrint x
  pPrint (String x) = Pretty.doubleQuotes $ pPrint x
  pPrint (Var x) = pPrint x

type Tag = Int

type Var = Id Type

type TyVar = Id ()

data Type
  = TInt32
  | TInt64
  | TFloat
  | TDouble
  | TChar
  | TString
  | TFun [Type] Type
  | TNode [(Tag, [Type])]
  | TPtr Type
  | TEmpty -- for typeUnion and Update
  | TVar TyVar
  | TForall [TyVar] Type
  | TRec TyVar Type
  deriving stock (Show, Eq, Ord, Generic)

instance Pretty Type where
  pPrint TInt32 = "Int32#"
  pPrint TInt64 = "Int64#"
  pPrint TFloat = "Float#"
  pPrint TDouble = "Double#"
  pPrint TChar = "Char#"
  pPrint TString = "String#"
  pPrint (TFun params ret) = parens (hsep (punctuate "," $ map pPrint params) <+> "->" <+> pPrint ret)
  pPrint (TNode ns) = "<" <> sep (punctuate "|" $ map (\(tag, ts) -> pPrint tag <> ":" <> hsep (map pPrint ts)) ns) <> ">"
  pPrint (TPtr ty) = "Ptr#" <> parens (pPrint ty)
  pPrint TEmpty = "Empty#"
  pPrint (TVar v) = pPrint v
  pPrint (TForall ts t) = "∀" <> sep (map pPrint ts) <> "." <+> pPrint t
  pPrint (TRec v t) = "μ" <> pPrint v <> "." <+> pPrint t

class HasType a where
  typeOf :: a -> Type

instance HasType Func where
  typeOf (Func ts ps e) = TForall ts $ TFun (map (view idMeta) ps) (typeOf e)

instance HasType Exp where
  typeOf (Bind _ _ e) = typeOf e
  typeOf (Unit v) = typeOf v
  typeOf (Match _ ((_, e) : _)) = typeOf e
  typeOf (Match _ []) = TEmpty
  typeOf (Apply _ (view idMeta -> TFun _ ret) _) = ret
  typeOf Apply {} = bug $ Unreachable "Apply can only be applied to TFun."
  typeOf (Alloc v) = TPtr $ typeOf v
  typeOf (Fetch (view idMeta -> TPtr ty) Nothing) = ty
  typeOf Fetch {} = bug $ Unreachable "Fetch can only be applied to TPtr."
  typeOf Update {} = TEmpty
  typeOf (Closure func) = typeOf func
  typeOf (TyAbs ts e) = TForall ts $ typeOf e
  typeOf (TyApply x ts) =
    case x ^. idMeta of
      TForall vs t -> applySubst (zip vs ts) t
      _ -> bug $ Unreachable "TyApply can only be applied to TForall."
  typeOf (Fold t _) = t
  typeOf (Unfold (TRec x t) _) = applySubst [(x, t)] t
  typeOf (Unfold _ _) = bug $ Unreachable "Unfold can only be applied to TRec."
  typeOf (Exit t) = t
  typeOf (Catch e1 _) = typeOf e1

applySubst :: Foldable t => t (TyVar, Type) -> Type -> Type
applySubst xs t = foldr applySubst1 t xs

applySubst1 :: (TyVar, Type) -> Type -> Type
applySubst1 (a, x) (TVar v)
  | a == v = x
  | otherwise = TVar v
applySubst1 (a, x) (TRec v t)
  -- [X -> _](μX.T) = T
  | a == v = TRec v t
  | otherwise = TRec v (applySubst1 (a, x) t)
applySubst1 subst (TFun ps r) = TFun (map (applySubst1 subst) ps) (applySubst1 subst r)
applySubst1 subst (TNode ns) = TNode (map (over (_2 . mapped) (applySubst1 subst)) ns)
applySubst1 subst (TPtr t) = TPtr (applySubst1 subst t)
applySubst1 subst (TForall vs t) = TForall vs (applySubst1 subst t) -- dom subst ∧ vs == Φ
applySubst1 _ t = t

instance HasType Value where
  typeOf (Pack ty _ _) = ty
  typeOf Int32 {} = TInt32
  typeOf Int64 {} = TInt64
  typeOf Float {} = TFloat
  typeOf Double {} = TDouble
  typeOf Char {} = TChar
  typeOf String {} = TString
  typeOf (Var var) = var ^. idMeta

newtype ProgramBuilderT m a = ProgramBuilderT {unProgramBuilderT :: StateT (Program, HashSet Var) m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadTrans, MonadWriter w, MonadReader r)

runProgramBuilderT :: Monad m => ProgramBuilderT m () -> m Program
runProgramBuilderT m = fst <$> execStateT (unProgramBuilderT m) (Program {topVars = [], topFuncs = [], externals = []}, mempty)

program :: Monad m => ProgramBuilderT m () -> m Program
program = runProgramBuilderT

class Monad m => MonadProgramBuilder m where
  getDefinedVars :: m (HashSet Var)
  preDef :: Var -> m ()
  defVar :: Var -> Exp -> m ()
  defFunc :: Var -> Func -> m ()
  defExt :: Var -> String -> m ()

instance Monad m => MonadProgramBuilder (ProgramBuilderT m) where
  getDefinedVars = ProgramBuilderT $ HashSet.union <$> gets (definedVars . fst) <*> gets snd
  preDef var = ProgramBuilderT $ modify $ second $ HashSet.insert var
  defVar name expr = ProgramBuilderT $ modify $ first (\p -> p {topVars = topVars p <> [(name, expr)]})
  defFunc name func = ProgramBuilderT $ modify $ first (\p -> p {topFuncs = topFuncs p <> [(name, func)]})
  defExt name raw = ProgramBuilderT $ modify $ first (\p -> p {externals = externals p <> [(name, raw)]})

newtype ExpBuilderT m a = ExpBuilderT {unExpBuilderT :: WriterT (Endo Exp) m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadIO, MonadTrans, MonadState s, MonadReader r)

runExpBuilderT :: Functor f => ExpBuilderT f Exp -> f Exp
runExpBuilderT m = uncurry (flip appEndo) <$> runWriterT (unExpBuilderT m)

exp :: Functor f => ExpBuilderT f Exp -> f Exp
exp = runExpBuilderT

class Monad m => MonadExpBuilder m where
  bind :: String -> Exp -> m Var

instance (MonadIO m, HasUniqSupply env, MonadReader env m) => MonadExpBuilder (ExpBuilderT m) where
  bind hint exp = do
    x <- newInternalId ("$" <> hint) (typeOf exp)
    ExpBuilderT $ tell $ Endo $ Bind exp x
    pure x

instance MonadProgramBuilder m => MonadProgramBuilder (ExpBuilderT m) where
  getDefinedVars = lift getDefinedVars
  preDef var = lift $ preDef var
  defVar name expr = lift $ defVar name expr
  defFunc name func = lift $ defFunc name func
  defExt name raw = lift $ defExt name raw

unit :: MonadExpBuilder m => Value -> m Var
unit value = bind "unit" (Unit value)

match :: MonadExpBuilder m => Value -> [(Value, Exp)] -> m Var
match x cs = bind "match" (Match x cs)

apply :: MonadExpBuilder m => ApplyMode -> Var -> [Value] -> m Var
apply mode f xs = bind "apply" (Apply mode f xs)

alloc :: MonadExpBuilder m => Value -> m Var
alloc init = bind "alloc" (Alloc init)

fetch :: MonadExpBuilder m => Var -> Maybe (Tag, Int) -> m Var
fetch x idx = bind "fetch" (Fetch x idx)

update :: MonadExpBuilder m => Var -> Value -> m Var
update x value = bind "update" (Update x value)

closure :: MonadExpBuilder m => Func -> m Var
closure func = bind "closure" (Closure func)

tyabs :: (MonadIO m, HasUniqSupply env, MonadReader env m, MonadExpBuilder m) => [String] -> ([TyVar] -> ExpBuilderT m Exp) -> m Var
tyabs hints k = do
  vs <- traverse (`newInternalId` ()) hints
  e' <- exp $ k vs
  bind "tyabs" $ TyAbs vs e'

tyapply :: MonadExpBuilder m => Var -> [Type] -> m Var
tyapply x ts = bind "tyapply" (TyApply x ts)

fold :: MonadExpBuilder m => Type -> Value -> m Var
fold t v = bind "fold" (Fold t v)

unfold :: MonadExpBuilder m => Type -> Value -> m Var
unfold t v = bind "unfold" (Unfold t v)
