module Language.Malgo.FrontEnd.Rename where

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

lookupId' :: SrcSpan -> Text -> ReaderT (Map Text Id) MalgoM Id
lookupId' ss name = do
  assertDefined ss name
  Just name' <- lookupId name
  return name'

-- Renamers
rename :: [Decl Text] -> MalgoM [Decl Id]
rename xs = usingReaderT mempty $ do
  headers <- Map.fromList . catMaybes <$> mapM header xs
  local (headers <>) $ mapM renameDecl xs
 where
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
      Nothing -> do
        new <- newId f
        return $ Just (f, new)
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
