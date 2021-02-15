{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.Malgo.NewTypeCheck.Pass (typeCheck) where

import Data.Fix
import qualified Data.Map as Map
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.KindF
import Language.Malgo.Prelude
import Language.Malgo.Rename.RnEnv (RnEnv)
import qualified Language.Malgo.Rename.RnEnv as R
import Language.Malgo.Syntax hiding (Type (..), freevars)
import qualified Language.Malgo.Syntax as S
import Language.Malgo.Syntax.Extension
import Language.Malgo.Type (PrimT (..))
import Language.Malgo.TypeF
import Language.Malgo.Unify
import Text.Megaparsec (SourcePos)

data TcEnv = TcEnv
  { _varEnv :: Map RnId (TypeScheme UKind),
    _typeEnv :: Map RnTId TypeDef,
    _rnEnv :: RnEnv
  }

data TypeDef = TypeDef
  { _typeConstructor :: UType,
    _typeParameters :: [Fix (TypeF UKind)],
    _valueConstructors :: [(RnId, TypeScheme UKind)]
  }

makeLenses ''TcEnv
makeLenses ''TypeDef

-------------------------------
-- Lookup the value of TcEnv --
-------------------------------

lookupVar :: (HasCallStack, MonadState TcEnv m, MonadMalgo m, MonadIO m) => SourcePos -> RnId -> m (TypeScheme UKind)
lookupVar pos name = do
  mscheme <- use $ varEnv . at name
  case mscheme of
    Nothing -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)
    Just scheme -> pure scheme

lookupType :: (MonadState TcEnv m, MonadMalgo m, MonadIO m) => SourcePos -> Id ModuleName -> m UType
lookupType pos name = do
  mtype <- preuse $ typeEnv . at name . _Just . typeConstructor
  case mtype of
    Nothing -> errorOn pos $ "Not in scope:" <+> quotes (pPrint name)
    Just typ -> pure typ

typeCheck :: (MonadUniq m, MonadMalgo m, MonadIO (TypeUnifyT m)) => RnEnv -> Module (Malgo 'Rename) -> m (Module (Malgo 'NewTypeCheck), TcEnv)
typeCheck rnEnv (Module name bg) = runKindUnifyT $
  runTypeUnifyT $ do
    let tcEnv = TcEnv mempty mempty rnEnv
    (bg', tcEnv') <- runStateT (tcBindGroup bg) tcEnv
    pure (Module name bg', tcEnv')

tcBindGroup :: (MonadBind (TypeF UKind) (TypeVar UKind) m, MonadState TcEnv m, MonadUniq m, MonadMalgo m, MonadIO m) => BindGroup (Malgo 'Rename) -> m (BindGroup (Malgo 'NewTypeCheck))
tcBindGroup bindGroup = do
  imports' <- tcImports $ bindGroup ^. imports
  dataDefs' <- tcDataDefs $ bindGroup ^. dataDefs
  foreigns' <- tcForeigns $ bindGroup ^. foreigns
  scSigs' <- tcScSigs $ bindGroup ^. scSigs
  traverse_ prepareTcScDefs $ bindGroup ^. scDefs
  scDefs' <- tcScDefGroup $ bindGroup ^. scDefs
  pure
    BindGroup
      { _dataDefs = dataDefs',
        _infixs = [],
        _foreigns = foreigns',
        _scSigs = scSigs',
        _scDefs = scDefs',
        _imports = imports'
      }

tcImports :: [Import (Malgo 'Rename)] -> m [Import (Malgo 'NewTypeCheck)]
tcImports = error "not implemented"

tcDataDefs :: (MonadBind (TypeF UKind) (TypeVar UKind) m, MonadState TcEnv m, MonadUniq m, MonadMalgo m, MonadIO m) => [DataDef (Malgo 'Rename)] -> m [DataDef (Malgo 'NewTypeCheck)]
tcDataDefs ds = do
  -- 相互再帰的な型定義がありうるため、型コンストラクタに対応するTyConを先にすべて生成する
  for_ ds $ \(_, name, params, _) -> do
    tyCon <- UTerm . TyCon <$> newGlobalId (name ^. idName) (buildTyConKind params)
    typeEnv . at name .= Just (TypeDef tyCon [] [])
  for ds $ \(pos, name, params, valueCons) -> do
    for_ params $ \p -> do
      p' <- UVar <$> freshVar @(TypeF UKind)
      typeEnv . at p .= Just (TypeDef p' [] [])
    valueCons' <- forOf (traversed . _2) valueCons $ \args -> do
      -- 値コンストラクタの型を構築
      name' <- lookupType pos name
      params' <- traverse (lookupType pos) params
      args' <- traverse transType args
      pure $ foldr (\l r -> UTerm $ TyArr l r) (foldr (\l r -> UTerm $ TyApp r l) name' params') args'
    let valueConsNames = map fst valueCons'
    let valueConsTypes = map snd valueCons'
    (as, valueConsTypes') <- generalizeUTermMutRecs pos mempty valueConsTypes
    let valueCons'' = zip valueConsNames valueConsTypes'
    varEnv <>= Map.fromList valueCons''
    typeEnv . at name %= (_Just . typeParameters .~ as) . (_Just . valueConstructors .~ valueCons'')
    pure (pos, name, params, map (second (map tcType)) valueCons)
  where
    buildTyConKind [] = UTerm $ Type BoxedRep
    buildTyConKind (_ : xs) = UTerm $ KArr (UTerm $ Type BoxedRep) (buildTyConKind xs)

tcForeigns :: [Foreign (Malgo 'Rename)] -> m [Foreign (Malgo 'NewTypeCheck)]
tcForeigns = error "not implemented"

tcScSigs :: [ScSig (Malgo 'Rename)] -> m [ScSig (Malgo 'NewTypeCheck)]
tcScSigs = error "not implemented"

prepareTcScDefs :: [ScDef (Malgo 'Rename)] -> m ()
prepareTcScDefs = error "not implemented"

tcScDefGroup :: [[ScDef (Malgo 'Rename)]] -> m [[ScDef (Malgo 'NewTypeCheck)]]
tcScDefGroup = error "not implemented"

-----------------------------------
-- Translate Type representation --
-----------------------------------

transType :: (MonadMalgo m, MonadState TcEnv m, MonadIO m) => S.Type (Malgo 'Rename) -> m UType
transType (S.TyApp _ t ts) = do
  rnEnv <- use rnEnv
  let ptr_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Ptr#" (view R.typeEnv rnEnv)
  case (t, ts) of
    (S.TyCon _ c, [t]) | c == ptr_t -> do
      t' <- transType t
      pure $ UTerm $ TyPtr t'
    _ -> foldr (\l r -> UTerm $ TyApp r l) <$> transType t <*> traverse transType ts
transType (S.TyVar pos v) = lookupType pos v
transType (S.TyCon pos c) = do
  rnEnv <- use rnEnv
  -- lookup RnTId of primitive types
  let int32_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Int32#" (view R.typeEnv rnEnv)
  let int64_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Int64#" (view R.typeEnv rnEnv)
  let float_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Float#" (view R.typeEnv rnEnv)
  let double_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Double#" (view R.typeEnv rnEnv)
  let char_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "Char#" (view R.typeEnv rnEnv)
  let string_t = fromJust $ find ((== ModuleName "Builtin") . view idMeta) =<< Map.lookup "String#" (view R.typeEnv rnEnv)
  if
      | c == int32_t -> pure $ UTerm $ TyPrim Int32T
      | c == int64_t -> pure $ UTerm $ TyPrim Int64T
      | c == float_t -> pure $ UTerm $ TyPrim FloatT
      | c == double_t -> pure $ UTerm $ TyPrim DoubleT
      | c == char_t -> pure $ UTerm $ TyPrim CharT
      | c == string_t -> pure $ UTerm $ TyPrim StringT
      | otherwise -> lookupType pos c
transType (S.TyArr _ t1 t2) = UTerm <$> (TyArr <$> transType t1 <*> transType t2)
transType (S.TyTuple _ ts) = UTerm <$> (TyTuple <$> traverse transType ts)
transType (S.TyLazy _ t) = UTerm <$> (TyLazy <$> transType t)

tcType :: S.Type (Malgo 'Rename) -> S.Type (Malgo 'NewTypeCheck)
tcType (S.TyApp pos t ts) = S.TyApp pos (tcType t) (map tcType ts)
tcType (S.TyVar pos v) = S.TyVar pos v
tcType (S.TyCon pos c) = S.TyCon pos c
tcType (S.TyArr pos t1 t2) = S.TyArr pos (tcType t1) (tcType t2)
tcType (S.TyTuple pos ts) = S.TyTuple pos $ map tcType ts
tcType (S.TyLazy pos t) = S.TyLazy pos $ tcType t
