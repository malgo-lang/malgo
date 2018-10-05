{-# LANGUAGE TupleSections #-}
module Language.Malgo.FrontEnd.Rename
  ( rename
  , renameDecl
  , renameExpr
  )
where

import qualified Data.Map                      as Map
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.IR.AST
import           Language.Malgo.Monad
import           Language.Malgo.Pretty
import           Universum

-- Types
data Id = Id { _name :: Text, _uniq :: Int }
  deriving (Show, Eq, Ord)

type RenameM a = ReaderT (Map Text Id) MalgoM a

-- Functions

-- Utilities
newId :: MonadMalgo m => Text -> m Id
newId name = Id name <$> newUniq

lookupId :: Text -> RenameM (Maybe Id)
lookupId name = Map.lookup name <$> ask

assertDefined :: SrcSpan -> Text -> RenameM ()
assertDefined ss name = do
  name' <- lookupId name
  whenNothing_ name'
    $   error
    $   show
    $   pPrint ss
    <+> ":"
    <+> pPrint name
    <+> "is not defined"

lookupId' :: SrcSpan -> Text -> RenameM Id
lookupId' ss name = do
  assertDefined ss name
  Just name' <- lookupId name
  return name'

-- Renamers
rename :: [Decl Text] -> MalgoM [Decl Id]
rename xs = usingReaderT mempty $ do
  headers <- Map.fromList . catMaybes <$> mapM header xs
  local (headers <>) $ mapM renameDecl xs

header :: Decl Text -> RenameM (Maybe (Text, Id))
header (ScDef ss f _ _) = do
  f' <- lookupId f
  case f' of
    Just _ -> return Nothing
    Nothing ->
      error
        $   show
        $   pPrint ss
        <+> ":"
        <+> "Type annotation for"
        <+> pPrint f
        <+> "is needed"
header (ScAnn ss f _) = do
  f' <- lookupId f
  case f' of
    Just _ ->
      error
        $   show
        $   pPrint ss
        <+> ":"
        <+> "Type annotation for"
        <+> pPrint f
        <+> "is already declared"
    Nothing -> Just . (f, ) <$> newId f
header _ = return Nothing

renameDecl :: Decl Text -> RenameM (Decl Id)
renameDecl (ScAnn ss x t   ) = ScAnn ss <$> lookupId' ss x <*> pure t
renameDecl (ScDef ss x ps e) = do
  x'  <- lookupId' ss x
  ps' <- mapM newId ps
  e'  <- local ((Map.fromList $ zip ps ps') <>) $ renameExpr e
  return $ ScDef ss x' ps' e'
renameDecl (AliasDef ss x ps t) = return $ AliasDef ss x ps t
renameDecl (TypeDef  ss x ps t) = return $ TypeDef ss x ps t

renameExpr :: Expr Text -> RenameM (Expr Id)
renameExpr (Var     ss a) = Var ss <$> lookupId' ss a
renameExpr (Literal ss x) = return $ Literal ss x
renameExpr (Record ss xs) =
  Record ss <$> mapM (\(l, v) -> (l, ) <$> renameExpr v) xs
renameExpr (Variant ss l v t) = Variant ss l <$> renameExpr v <*> pure t
renameExpr (Let ss bind e   ) = do
  (newBinds, bind') <- renameBind bind
  local (newBinds <>) $ Let ss bind' <$> renameExpr e
renameExpr (Apply ss e1 e2) = Apply ss <$> renameExpr e1 <*> renameExpr e2
renameExpr (Case ss e clauses) =
  Case ss <$> renameExpr e <*> mapM renameClause clauses
renameExpr (Fn ss params e) = do
  xs <- mapM (newId . view _1) params
  local (Map.fromList (zip (map (view _1) params) xs) <>)
    $   Fn ss (zip xs (map (view _2) params))
    <$> renameExpr e

renameBind :: Bind Text -> RenameM (Map Text Id, Bind Id)
renameBind (NonRec ss n t e) = do
  e' <- renameExpr e
  n' <- newId n
  return (Map.singleton n n', NonRec ss n' t e')
renameBind (Rec bs) = do
  let fs = map (view _2) bs
  fs' <- mapM newId fs
  let newBinds = Map.fromList (zip fs fs')
  local (newBinds <>) $ (newBinds, ) . Rec <$> mapM renameRec bs
 where
  renameRec (ss, f, mt, ps, e) = do
    f'  <- lookupId' ss f
    ps' <- mapM newId ps
    local (Map.fromList (zip ps ps') <>) $ (ss, f', mt, ps', ) <$> renameExpr e

renameClause :: Clause Text -> RenameM (Clause Id)
renameClause (VariantPat ss l x t e) = do
  x' <- newId x
  local (Map.singleton x x' <>) $ VariantPat ss l x' t <$> renameExpr e
renameClause (BoolPat ss b e) = BoolPat ss b <$> renameExpr e
renameClause (VarPat  ss x e) = do
  x' <- newId x
  local (Map.singleton x x' <>) $ VarPat ss x' <$> renameExpr e
