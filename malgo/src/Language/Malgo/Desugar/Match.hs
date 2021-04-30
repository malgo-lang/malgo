{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | パターンマッチのコンパイル
module Language.Malgo.Desugar.Match (match, PatMatrix, patMatrix) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Koriel.Core.Syntax
import qualified Koriel.Core.Syntax as Core
import Koriel.Core.Type
import qualified Koriel.Core.Type as Core
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Language.Malgo.Desugar.DsEnv
import Language.Malgo.Desugar.Type (dsType, unfoldType)
import Language.Malgo.Desugar.Unboxed (dsUnboxed)
import Language.Malgo.Prelude
import Language.Malgo.Syntax
import Language.Malgo.Syntax.Extension
import Language.Malgo.TypeRep.Static
import qualified Language.Malgo.TypeRep.Static as Malgo

-- TODO: The Implementation of Functional Programming Languages
-- を元にコメントを追加

-- 各節のパターン列を行列に見立て、転置してmatchにわたし、パターンを分解する
-- 例えば、{ f Nil -> f empty | f (Cons x xs) -> f x }の場合は、
-- [ [f, Nil], [f, Cons x xs] ] に見立て、
-- [ [f, f], [Nil, Cons x xs] ] に転置する

newtype PatMatrix = PatMatrix
  { -- | パターンの転置行列
    innerList :: [[Pat (Malgo 'Refine)]]
  }
  deriving stock (Show)
  deriving newtype (Pretty)

patMatrix :: [[Pat (Malgo 'Refine)]] -> PatMatrix
patMatrix xss = PatMatrix $ List.transpose xss

headCol :: PatMatrix -> Maybe [Pat (Malgo 'Refine)]
headCol PatMatrix {innerList = []} = Nothing
headCol PatMatrix {innerList = x : _} = Just x

tailCol :: PatMatrix -> PatMatrix
tailCol PatMatrix {innerList = []} = PatMatrix []
tailCol PatMatrix {innerList = _ : xs} = PatMatrix xs

consCol :: [Pat (Malgo 'Refine)] -> PatMatrix -> PatMatrix
consCol ps PatMatrix {..} = PatMatrix (ps : innerList)

splitCol :: PatMatrix -> (Maybe [Pat (Malgo 'Refine)], PatMatrix)
splitCol mat = (headCol mat, tailCol mat)

-- パターンマッチを分解し、switch-case相当の分岐で表現できるように変換する
match ::
  HasCallStack =>
  (MonadState DsEnv m, MonadIO m, MonadUniq m, MonadFail m) =>
  -- | マッチ対象
  [Id Core.Type] ->
  -- | パターン（転置行列）
  PatMatrix ->
  -- | righthand
  [m (Core.Exp (Id Core.Type))] ->
  -- | fail
  Core.Exp (Id Core.Type) ->
  m (Core.Exp (Id Core.Type))
match (u : us) (splitCol -> (Just ps, pss)) es err
  -- Variable Rule
  -- パターンの先頭がすべて変数のとき
  | all (has _VarP) ps = do
    -- 変数パターンvについて、式中に現れるすべてのvをパターンマッチ対象のuで置き換える
    let es' =
          zipWith
            ( \case
                (VarP _ v) -> \e -> nameEnv . at v ?= u >> e
                _ -> bug Unreachable
            )
            ps
            es
    match us pss es' err
  -- Constructor Rule
  -- パターンの先頭がすべて値コンストラクタのとき
  | all (has _ConP) ps = do
    patType <- Malgo.typeOf $ head ps
    unless (Malgo._TyApp `has` patType || Malgo._TyCon `has` patType) $
      errorDoc $ "Not valid type:" <+> pPrint patType
    -- 型からコンストラクタの集合を求める
    let (con, ts) = Malgo.splitCon patType
    vcs <- lookupValueConstructors con ts
    -- 各コンストラクタごとにC.Caseを生成する
    cases <- for vcs \(conName, Forall _ conType) -> do
      paramTypes <- traverse dsType $ fst $ splitTyArr conType
      let ccon = Core.Con (Data $ conName ^. toText) paramTypes
      params <- traverse (newLocalId "$p") paramTypes
      let (pss', es') = group conName (consCol ps pss) es
      Unpack ccon params <$> match (params <> us) pss' es' err
    unfoldedType <- unfoldType patType
    pure $ Match (Cast unfoldedType $ Core.Var u) $ NonEmpty.fromList cases
  -- パターンの先頭がすべてレコードのとき
  | all (has _RecordP) ps = do
    patType <- Malgo.typeOf $ head ps
    SumT [con@(Core.Con _ ts)] <- dsType patType
    undefined
  -- パターンの先頭がすべてタプルのとき
  | all (has _TupleP) ps = do
    patType <- Malgo.typeOf $ head ps
    SumT [con@(Core.Con Core.Tuple ts)] <- dsType patType
    params <- traverse (newLocalId "$p") ts
    cases <- do
      let (pss', es') = groupTuple (consCol ps pss) es
      (:| []) . Unpack con params <$> match (params <> us) pss' es' err
    pure $ Match (Atom $ Core.Var u) cases
  -- パターンの先頭がすべてunboxedな値のとき
  | all (has _UnboxedP) ps = do
    let cs =
          map
            ( \case
                UnboxedP _ x -> dsUnboxed x
                _ -> bug Unreachable
            )
            ps
    cases <- traverse (\c -> Switch c <$> match us pss es err) cs
    -- パターンの網羅性を保証するため、
    -- `_ -> err` を追加する
    hole <- newLocalId "$_" (Core.typeOf u)
    pure $ Match (Atom $ Core.Var u) $ NonEmpty.fromList (cases <> [Core.Bind hole err])
  -- The Mixture Rule
  -- 複数種類のパターンが混ざっているとき
  | otherwise =
    do
      let ((ps', ps''), (pss', pss''), (es', es'')) = partition ps pss es
      err' <- match (u : us) (consCol ps'' pss'') es'' err
      match (u : us) (consCol ps' pss') es' err'
match [] (PatMatrix []) (e : _) _ = e
match _ (PatMatrix []) [] err = pure err
match us pss es err = do
  errorDoc $ "match" <+> pPrint us <+> pPrint pss <+> pPrint (length es) <+> pPrint err

-- Mixture Rule以外にマッチするようにパターン列を分解
partition ::
  [Pat (Malgo 'Refine)] ->
  PatMatrix ->
  [m (Core.Exp (Id Core.Type))] ->
  ( ([Pat (Malgo 'Refine)], [Pat (Malgo 'Refine)]),
    (PatMatrix, PatMatrix),
    ([m (Core.Exp (Id Core.Type))], [m (Core.Exp (Id Core.Type))])
  )
partition [] _ _ = bug Unreachable
partition ps@(VarP {} : _) (PatMatrix pss) es =
  let (ps', ps'') = span (has _VarP) ps
   in ((ps', ps''), bimap PatMatrix PatMatrix $ unzip $ map (splitAt (length ps')) pss, splitAt (length ps') es)
partition ps@(ConP {} : _) (PatMatrix pss) es =
  let (ps', ps'') = span (has _ConP) ps
   in ((ps', ps''), bimap PatMatrix PatMatrix $ unzip $ map (splitAt (length ps')) pss, splitAt (length ps') es)
partition ps@(TupleP {} : _) (PatMatrix pss) es =
  let (ps', ps'') = span (has _TupleP) ps
   in ((ps', ps''), bimap PatMatrix PatMatrix $ unzip $ map (splitAt (length ps')) pss, splitAt (length ps') es)
partition ps@(RecordP {} : _) (PatMatrix pss) es =
  let (ps', ps'') = span (has _RecordP) ps
   in ((ps', ps''), bimap PatMatrix PatMatrix $ unzip $ map (splitAt (length ps')) pss, splitAt (length ps') es)
partition ps@(UnboxedP {} : _) (PatMatrix pss) es =
  let (ps', ps'') = span (has _UnboxedP) ps
   in ((ps', ps''), bimap PatMatrix PatMatrix $ unzip $ map (splitAt (length ps')) pss, splitAt (length ps') es)

-- コンストラクタgconの引数部のパターンpsを展開したパターン行列を生成する
group ::
  XId (Malgo 'Refine) ->
  PatMatrix ->
  [m (Core.Exp (Id Core.Type))] ->
  (PatMatrix, [m (Core.Exp (Id Core.Type))])
group gcon (PatMatrix (List.transpose -> pss)) es = over _1 patMatrix $ unzip $ mapMaybe (aux gcon) (zip pss es)
  where
    aux gcon (ConP _ gcon' ps : pss, e)
      | gcon == gcon' = Just (ps <> pss, e)
      | otherwise = Nothing
    aux _ (p : _, _) = errorDoc $ "Invalid pattern:" <+> pPrint p
    aux _ ([], _) = bug Unreachable

groupTuple :: PatMatrix -> [m (Core.Exp (Id Core.Type))] -> (PatMatrix, [m (Core.Exp (Id Core.Type))])
groupTuple (PatMatrix (List.transpose -> pss)) es = over _1 patMatrix $ unzip $ zipWith aux (List.transpose pss) es
  where
    aux (TupleP _ ps : pss) e = (ps <> pss, e)
    aux (p : _) _ = errorDoc $ "Invalid pattern:" <+> pPrint p
    aux [] _ = bug Unreachable
