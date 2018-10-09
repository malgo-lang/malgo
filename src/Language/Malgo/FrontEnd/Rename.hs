{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}
module Language.Malgo.FrontEnd.Rename
  ( initRnEnv
  , rename
  , renameDecl
  , renameType
  , renameExpr
  , Id
  , RnEnv(..)
  )
where

import qualified Data.Map                    as Map
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.Id
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Universum

-- Types
data RnEnv = RnEnv { _varmap :: Map Text Id
                   , _tymap  :: Map Text Id
                   }

type RenameM a = ReaderT RnEnv MalgoM a

initRnEnv :: MalgoM RnEnv
initRnEnv = do
  let tyPrims = ["->", "Int", "Float", "Char"]
  tyPrimIds <- mapM newId tyPrims
  return $ RnEnv mempty (Map.fromList (zip tyPrims tyPrimIds))

-- Functions

-- Utilities

varmap :: Functor f => (Map Text Id -> f (Map Text Id)) -> RnEnv -> f RnEnv
varmap f (RnEnv v t) = (\v' -> RnEnv v' t) <$> f v

tymap :: Functor f => (Map Text Id -> f (Map Text Id)) -> RnEnv -> f RnEnv
tymap f (RnEnv v t) = (\t' -> RnEnv v t') <$> f t

lookupId :: Lens' RnEnv (Map Text Id) -> Text -> RenameM (Maybe Id)
lookupId l name = Map.lookup name <$> view l

assertDefined :: Lens' RnEnv (Map Text Id) -> SrcSpan -> Text -> RenameM ()
assertDefined l ss name = do
  name' <- lookupId l name
  whenNothing_ name'
    $   error
    $   show
    $   pPrint ss
    <+> ":"
    <+> pPrint name
    <+> "is not defined"

lookupId' :: Lens' RnEnv (Map Text Id) -> SrcSpan -> Text -> RenameM Id
lookupId' l ss name = do
  assertDefined l ss name
  Just name' <- lookupId l name
  return name'

-- Renamers
rename :: RnEnv -> [Decl Text] -> MalgoM ([Decl Id])
rename rnEnv xs = usingReaderT rnEnv $ do
  scHeaders <- Map.fromList . catMaybes <$> mapM scHeader xs
  tyHeaders <- Map.fromList . catMaybes <$> mapM tyHeader xs
  local (over varmap (scHeaders <>))
    $ local (over tymap (tyHeaders <>))
    $ mapM renameDecl xs

scHeader :: Decl Text -> RenameM (Maybe (Text, Id))
scHeader (ScDef _ f _ _) = do
  f' <- lookupId varmap f
  case f' of
    Just _  -> return Nothing
    Nothing -> Just . (f, ) <$> newId f
scHeader (ScAnn ss f _) = do
  f' <- lookupId varmap f
  case f' of
    Just _ ->
      error $ show $ pPrint ss <+> ":" <+> "Type annotation for"
      <+> pPrint f <+> "is already declared"
    Nothing -> Just . (f, ) <$> newId f
scHeader _ = return Nothing

tyHeader :: Decl Text -> RenameM (Maybe (Text, Id))
tyHeader (TypeDef ss name _ _) = do
  m <- lookupId tymap name
  case m of
    Just _ -> error $ show $ pPrint ss <+> ":"
      <+> pPrint name <+> "is already defined"
    Nothing -> Just . (name, ) <$> newId name
tyHeader _ = return Nothing

renameDecl :: Decl Text -> RenameM (Decl Id)
renameDecl (ScAnn ss x t) =
  ScAnn ss <$> lookupId' varmap ss x <*> renameType t
renameDecl (ScDef ss x ps e) = do
  x'  <- lookupId' varmap ss x
  ps' <- mapM newId ps
  e'  <- local (over varmap ((Map.fromList $ zip ps ps') <>))
         $ renameExpr e
  return $ ScDef ss x' ps' e'
renameDecl (TypeDef  ss x ps t) = do
  x' <- lookupId' tymap ss x
  ps' <- mapM newId ps
  t' <- local (over tymap ((Map.fromList $ zip ps ps') <>))
        $ renameType t
  return $ TypeDef ss x' ps' t'

renameType :: SType Text -> RenameM (SType Id)
renameType (STyApp ss (SimpleC _ name) args) = do
  name' <- lookupId' tymap ss name
  args' <- mapM renameType args
  return (STyApp ss (SimpleC ss name') args')
renameType (STyApp ss (SRecordC _ xs) []) = do
  xs' <- mapM (\(x, t) -> (x, ) <$> renameType t) xs
  return (STyApp ss (SRecordC ss (sortOn (view _1) xs')) [])
renameType (STyApp ss (SVariantC _ xs) []) = do
  xs' <- mapM (\(x, t) -> (x, ) <$> renameType t) xs
  return (STyApp ss (SVariantC ss (sortOn (view _1) xs')) [])
renameType (STyVar ss name) = STyVar ss <$> lookupId' tymap ss name
renameType _ = error "unreachable(renameType)"

renameExpr :: Expr Text -> RenameM (Expr Id)
renameExpr (Var ss a) = Var ss <$> lookupId' varmap ss a
renameExpr (Literal ss x) = return $ Literal ss x
renameExpr (Record ss xs) =
  Record ss <$> mapM (\(l, v) -> (l, ) <$> renameExpr v) xs
renameExpr (Variant ss l v ts) =
  Variant ss l <$> renameExpr v
  <*> mapM (\(x, t) -> (x, ) <$> renameType t) ts
renameExpr (Let ss bind e   ) = do
  (newBinds, bind') <- renameBind bind
  local (over varmap (newBinds <>)) $ Let ss bind' <$> renameExpr e
renameExpr (Apply ss e1 e2) = Apply ss <$> renameExpr e1 <*> renameExpr e2
renameExpr (Case ss e clauses) =
  Case ss <$> renameExpr e <*> mapM renameClause clauses
renameExpr (Fn ss params e) = do
  xs <- mapM (newId . view _1) params
  ts <- forM (map (view _2) params) $ \mty ->
    case mty of
      Just t  -> Just <$> renameType t
      Nothing -> return Nothing
  local (over varmap (Map.fromList (zip (map (view _1) params) xs) <>))
    $ Fn ss (zip xs ts)
    <$> renameExpr e

renameBind :: Bind Text -> RenameM (Map Text Id, Bind Id)
renameBind (NonRec ss n t e) = do
  e' <- renameExpr e
  n' <- newId n
  t' <- case t of
          Just t' -> Just <$> renameType t'
          Nothing -> return Nothing
  return (Map.singleton n n', NonRec ss n' t' e')
renameBind (Rec bs) = do
  let fs = map (view _2) bs
  fs' <- mapM newId fs
  let newBinds = Map.fromList (zip fs fs')
  local (over varmap (newBinds <>)) $ (newBinds, ) . Rec
    <$> mapM renameRec bs
 where
  renameRec (ss, f, mt, ps, e) = do
    f'  <- lookupId' varmap ss f
    mt' <- case mt of
             Just t  -> Just <$> renameType t
             Nothing -> return Nothing
    ps' <- mapM newId ps
    local (over varmap (Map.fromList (zip ps ps') <>))
      $ (ss, f', mt', ps', ) <$> renameExpr e

renameClause :: Clause Text -> RenameM (Clause Id)
renameClause (VariantPat ss l x ts e) = do
  x' <- newId x
  local (over varmap (Map.singleton x x' <>))
    $ VariantPat ss l x'
    <$> mapM (\(lk, t) -> (lk,) <$> renameType t) ts
    <*> renameExpr e
renameClause (BoolPat ss b e) = BoolPat ss b <$> renameExpr e
renameClause (VarPat  ss x e) = do
  x' <- newId x
  local (over varmap (Map.singleton x x' <>))
    $ VarPat ss x' <$> renameExpr e
