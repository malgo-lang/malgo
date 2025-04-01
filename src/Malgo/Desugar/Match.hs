-- | パターンマッチのコンパイル
module Malgo.Desugar.Match (match, PatMatrix, patMatrix) where

import Control.Lens (Prism', has, _1)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Traversable (for)
import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State, modify)
import Malgo.Core.Syntax
import Malgo.Core.Syntax qualified as Core
import Malgo.Core.Type
import Malgo.Core.Type qualified as Core
import Malgo.Desugar.DsState
import Malgo.Desugar.Type (dsType, unfoldType)
import Malgo.Desugar.Unboxed (dsUnboxed)
import Malgo.Id
import Malgo.Infer.TypeRep
import Malgo.Infer.TypeRep qualified as Malgo
import Malgo.Module
import Malgo.MonadUniq (Uniq)
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension
import Prettyprinter ((<+>))

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
patMatrix xss = PatMatrix $ transpose xss

headCol :: PatMatrix -> Maybe [Pat (Malgo 'Refine)]
headCol PatMatrix {innerList = []} = Nothing
headCol PatMatrix {innerList = x : _} = Just x

tailCol :: PatMatrix -> PatMatrix
tailCol PatMatrix {innerList = []} = PatMatrix []
tailCol PatMatrix {innerList = _ : xs} = PatMatrix xs

-- consCol :: [Pat (Malgo 'Refine)] -> PatMatrix -> PatMatrix
-- consCol ps PatMatrix {..} = PatMatrix (ps : innerList)

splitCol :: PatMatrix -> (Maybe [Pat (Malgo 'Refine)], PatMatrix)
splitCol mat = (headCol mat, tailCol mat)

-- パターンマッチを分解し、switch-case相当の分岐で表現できるように変換する
match ::
  (State DsState :> es, Reader ModuleName :> es, State Uniq :> es) =>
  -- | マッチ対象
  [Meta Core.Type] ->
  -- | パターン（転置行列）
  PatMatrix ->
  -- | righthand
  [Eff es (Core.Expr (Meta Core.Type))] ->
  -- | fail
  Core.Expr (Meta Core.Type) ->
  Eff es (Core.Expr (Meta Core.Type))
match (scrutinee : restScrutinee) pat@(splitCol -> (Just heads@(head : _), tails)) es err
  -- Variable Rule
  -- パターンの先頭がすべて変数のとき
  | all (has _VarP) heads = do
      -- 変数パターンvについて、式中に現れるすべてのvをscrutineeで置き換える
      match
        restScrutinee
        tails
        ( zipWith
            ( \case
                (VarP _ v) -> \e -> modify (\s@DsState {..} -> s {nameEnv = Map.insert v scrutinee nameEnv}) >> e
                _ -> error "All elements of heads must be VarP"
            )
            heads
            es
        )
        err
  -- Constructor Rule
  -- パターンの先頭がすべて値コンストラクタのとき
  | all (has _ConP) heads = do
      let patType = Malgo.typeOf head
      -- unless (Malgo._TyApp `has` patType || Malgo._TyCon `has` patType) $
      --  errorDoc $ "Not valid type:" <+> pretty patType
      -- 型からコンストラクタの集合を求める
      let (con, ts) = case Malgo.viewTyConApp patType of
            Just (Malgo.TyCon con, ts) -> (con, ts)
            _ -> error "patType must be TyApp or TyCon"
      valueConstructors <- lookupValueConstructors con ts
      -- 各コンストラクタごとにC.Caseを生成する
      cases <- for valueConstructors \(conName, Forall _ conType) -> do
        paramTypes <- traverse dsType $ fst $ splitTyArr conType
        let coreCon = Core.Con (Data $ idToText conName) paramTypes
        params <- traverse (\t -> withMeta t <$> newTemporalId "p") paramTypes
        let (pat', es') = group conName pat es
        Unpack coreCon params <$> match (params <> restScrutinee) pat' es' err
      unfoldedType <- unfoldType patType
      pure $ Match (Cast unfoldedType $ Core.Var scrutinee) cases
  -- パターンの先頭がすべてレコードのとき
  | all (has _RecordP) heads = do
      let patType = Malgo.typeOf head
      dsType patType >>= \case
        RecordT kts -> do
          params <- traverse (\t -> withMeta t <$> newTemporalId "p") kts
          clause <- do
            (pat', es') <- groupRecord pat es
            OpenRecord params <$> match (Map.elems params <> restScrutinee) pat' es' err
          pure $ Match (Atom $ Core.Var scrutinee) [clause]
        _ -> error "patType must be RecordT"
  -- パターンの先頭がすべてタプルのとき
  | all (has _TupleP) heads = do
      let patType = Malgo.typeOf head
      dsType patType >>= \case
        SumT [con@(Core.Con Core.Tuple ts)] -> do
          params <- traverse (\t -> withMeta t <$> newTemporalId "p") ts
          clause <- do
            let (pat', es') = groupTuple pat es
            Unpack con params <$> match (params <> restScrutinee) pat' es' err
          pure $ Match (Atom $ Core.Var scrutinee) [clause]
        _ -> error "patType must be SumT [Tuple]"
  -- パターンの先頭がすべてunboxedな値のとき
  | all (has _UnboxedP) heads = do
      let cs =
            map
              ( \case
                  UnboxedP _ x -> dsUnboxed x
                  _ -> error "All elements of heads must be UnboxedP"
              )
              heads
      cases <- traverse (\c -> Exact c <$> match restScrutinee tails es err) cs
      -- パターンの網羅性を保証するため、
      -- `_ -> err` を追加する
      hole <- withMeta (Core.typeOf scrutinee) <$> newTemporalId "_"
      pure $ Match (Atom $ Core.Var scrutinee) $ cases <> [Core.Bind hole (Core.typeOf hole) err]
  -- The Mixture Rule
  -- 複数種類のパターンが混ざっているとき
  | otherwise =
      do
        let ((pat', pat''), (es', es'')) = partition pat es
        err' <- match (scrutinee : restScrutinee) pat'' es'' err
        match (scrutinee : restScrutinee) pat' es' err'
match [] (PatMatrix []) (e : _) _ = e
match _ (PatMatrix []) [] err = pure err
match scrutinees pat es err = do
  errorDoc $ "match" <+> pretty scrutinees <+> pretty pat <+> pretty (length es) <+> pretty err

-- Mixture Rule以外にマッチするようにパターン列を分解
-- [ [Cons A xs]
-- , [Cons x xs]
-- , [Nil] ]
-- ->
-- ( [ [Cons A xs]
--   , [Cons x xs] ]
-- , [ [Nil] ])
partition ::
  PatMatrix ->
  [m (Core.Expr (Meta Core.Type))] ->
  ( (PatMatrix, PatMatrix),
    ([m (Core.Expr (Meta Core.Type))], [m (Core.Expr (Meta Core.Type))])
  )
partition (splitCol -> (Just heads@(VarP {} : _), PatMatrix tails)) es = partitionOn _VarP heads tails es
partition (splitCol -> (Just heads@(ConP {} : _), PatMatrix tails)) es = partitionOn _ConP heads tails es
partition (splitCol -> (Just heads@(TupleP {} : _), PatMatrix tails)) es = partitionOn _TupleP heads tails es
partition (splitCol -> (Just heads@(RecordP {} : _), PatMatrix tails)) es = partitionOn _RecordP heads tails es
partition (splitCol -> (Just heads@(UnboxedP {} : _), PatMatrix tails)) es = partitionOn _UnboxedP heads tails es
partition _ _ = error "All patterns are covered"

partitionOn ::
  Prism' (Pat (Malgo 'Refine)) b ->
  [Pat (Malgo 'Refine)] ->
  [[Pat (Malgo 'Refine)]] ->
  [a] ->
  ((PatMatrix, PatMatrix), ([a], [a]))
partitionOn prism heads tails es =
  ( (PatMatrix $ onHeads : onTails, PatMatrix $ otherHeads : otherTails),
    List.splitAt (length onHeads) es
  )
  where
    -- onHeads : onTails => pattern that row starts with prism
    -- otherHeads : otherTails => pattern row that starts without prism
    (onHeads, otherHeads) = List.span (has prism) heads
    (onTails, otherTails) = unzip $ map (List.splitAt (length onHeads)) tails

-- コンストラクタgconの引数部のパターンpsを展開したパターン行列を生成する
group ::
  XId (Malgo 'Refine) ->
  PatMatrix ->
  [m (Core.Expr (Meta Core.Type))] ->
  (PatMatrix, [m (Core.Expr (Meta Core.Type))])
group gcon (PatMatrix (transpose -> pss)) es = over _1 patMatrix $ unzip $ mapMaybe (aux gcon) (zip pss es)
  where
    aux gcon (ConP _ gcon' ps : pss, e)
      | gcon == gcon' = Just (ps <> pss, e)
      | otherwise = Nothing
    aux _ (p : _, _) = errorDoc $ "Invalid pattern:" <+> pretty p
    aux _ ([], _) = error "ps must be not empty"

groupTuple :: PatMatrix -> [m (Core.Expr (Meta Core.Type))] -> (PatMatrix, [m (Core.Expr (Meta Core.Type))])
groupTuple (PatMatrix (transpose -> pss)) es = over _1 patMatrix $ unzip $ zipWith aux pss es
  where
    aux (TupleP _ ps : pss) e = (ps <> pss, e)
    aux (p : _) _ = errorDoc $ "Invalid pattern:" <+> pretty p
    aux [] _ = error "ps must be not empty"

groupRecord :: (State Uniq :> es, Reader ModuleName :> es) => PatMatrix -> [b] -> Eff es (PatMatrix, [b])
groupRecord (PatMatrix pss) es = over _1 patMatrix . unzip <$> zipWithM aux pss es
  where
    aux (RecordP x ps : pss) e = do
      ps' <- extendRecordP x ps
      pure (ps' <> pss, e)
    aux (p : _) _ = errorDoc $ "Invalid pattern:" <+> pretty p
    aux [] _ = error "ps must be not empty"
    extendRecordP (Typed (Malgo.TyRecord ktsMap) pos) ps = do
      let kts = Map.toList ktsMap
      for kts \(key, ty) ->
        case List.lookup key ps of
          Nothing -> VarP (Typed ty pos) <$> newTemporalId "_p"
          Just p -> pure p
    extendRecordP _ _ = error "typeOf x must be TyRecord"
