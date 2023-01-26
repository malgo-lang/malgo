-- | パターンマッチのコンパイル
module Malgo.Desugar.Match (match, PatMatrix, patMatrix) where

import Control.Lens (Prism', has, over, _1)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Traversable (for)
import Koriel.Core.Syntax
import Koriel.Core.Syntax qualified as Core
import Koriel.Core.Type
import Koriel.Core.Type qualified as Core
import Koriel.Id
import Koriel.Pretty
import Malgo.Desugar.DsEnv (DsEnv (uniqSupply))
import Malgo.Desugar.DsState
import Malgo.Desugar.Type (dsType, unfoldType)
import Malgo.Desugar.Unboxed (dsUnboxed)
import Malgo.Infer.TypeRep
import Malgo.Infer.TypeRep qualified as Malgo
import Malgo.Prelude hiding (group)
import Malgo.Syntax
import Malgo.Syntax.Extension

-- TODO: The Implementation of Functional Programming Languages
-- を元にコメントを追加

-- | 各節のパターン列を行列に見立て、転置してmatchにわたし、パターンを分解する
-- 例えば、{ f Nil -> f empty | f (Cons x xs) -> f x }の場合は、
-- [ [f, Nil], [f, Cons x xs] ] に見立て、
-- [ [f, f], [Nil, Cons x xs] ] に転置する
newtype PatMatrix = PatMatrix
  { -- | パターンの転置行列
    innerList :: [[Pat (Malgo 'Refine)]]
  }
  deriving stock (Show)
  deriving newtype (Pretty)

-- | PatMatrixのコンストラクタ
-- 転置前のパターン列を受け取り、転置してPatMatrixを作成する
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
  (MonadState DsState m, MonadReader DsEnv m, MonadIO m, MonadFail m) =>
  -- | マッチ対象
  [Id Core.Type] ->
  -- | パターン（転置行列）
  PatMatrix ->
  -- | righthand
  [m (Core.Exp (Id Core.Type))] ->
  -- | fail
  Core.Exp (Id Core.Type) ->
  m (Core.Exp (Id Core.Type))
match (scrutinee : restScrutinee) pat@(splitCol -> (Just heads, tails)) es err
  -- Variable Rule
  -- パターンの先頭がすべて変数のとき
  | all (has _VarP) heads = do
      -- 変数パターンvについて、式中に現れるすべてのvをscrutineeで置き換える
      match
        restScrutinee
        tails
        ( zipWith
            ( \case
                (VarP _ v) -> \e -> do
                  modify \s -> s {nameEnv = HashMap.insert v scrutinee s.nameEnv}
                  e
                _ -> error "All elements of heads must be VarP"
            )
            heads
            es
        )
        err
  -- Constructor Rule
  -- パターンの先頭がすべて値コンストラクタのとき
  | all (has _ConP) heads = do
      let patType = Malgo.typeOf $ List.head heads
      -- unless (Malgo._TyApp `has` patType || Malgo._TyCon `has` patType) $
      --  errorDoc $ "Not valid type:" <+> pPrint patType
      -- 型からコンストラクタの集合を求める
      let (con, ts) = case Malgo.viewTyConApp patType of
            Just (Malgo.TyCon con, ts) -> (con, ts)
            _ -> error "patType must be TyApp or TyCon"
      valueConstructors <- lookupValueConstructors con ts
      -- 各コンストラクタごとにC.Caseを生成する
      cases <- for valueConstructors \(conName, Forall _ conType) -> do
        paramTypes <- traverse dsType $ fst $ splitTyArr conType
        let coreCon = Core.Con (Data $ idToText conName) paramTypes
        params <- traverse (newTemporalId "p") paramTypes
        let (pat', es') = group conName pat es
        Unpack coreCon params <$> match (params <> restScrutinee) pat' es' err
      unfoldedType <- unfoldType patType
      pure $ Match (Cast unfoldedType $ Core.Var scrutinee) $ NonEmpty.fromList cases
  -- パターンの先頭がすべてレコードのとき
  | all (has _RecordP) heads = do
      let patType = Malgo.typeOf $ List.head heads
      RecordT kts <- dsType patType
      params <- traverse (newTemporalId "p") kts
      clause <- do
        (pat', es') <- groupRecord pat es
        OpenRecord params <$> match (HashMap.elems params <> restScrutinee) pat' es' err
      pure $ Match (Atom $ Core.Var scrutinee) $ clause :| []
  -- パターンの先頭がすべてタプルのとき
  | all (has _TupleP) heads = do
      let patType = Malgo.typeOf $ List.head heads
      SumT [con@(Core.Con Core.Tuple ts)] <- dsType patType
      params <- traverse (newTemporalId "p") ts
      clause <- do
        let (pat', es') = groupTuple pat es
        Unpack con params <$> match (params <> restScrutinee) pat' es' err
      pure $ Match (Atom $ Core.Var scrutinee) $ clause :| []
  -- パターンの先頭がすべてunboxedな値のとき
  | all (has _UnboxedP) heads = do
      let cs =
            map
              ( \case
                  UnboxedP _ x -> dsUnboxed x
                  _ -> error "All elements of heads must be UnboxedP"
              )
              heads
      cases <- traverse (\c -> Switch c <$> match restScrutinee tails es err) cs
      -- パターンの網羅性を保証するため、
      -- `_ -> err` を追加する
      hole <- newTemporalId "_" (Core.typeOf scrutinee)
      pure $ Match (Atom $ Core.Var scrutinee) $ NonEmpty.fromList (cases <> [Core.Bind hole err])
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
  errorDoc $ "match" <+> pPrint scrutinees <+> pPrint pat <+> pPrint (length es) <+> pPrint err

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
  [m (Core.Exp (Id Core.Type))] ->
  ( (PatMatrix, PatMatrix),
    ([m (Core.Exp (Id Core.Type))], [m (Core.Exp (Id Core.Type))])
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
  [m (Core.Exp (Id Core.Type))] ->
  (PatMatrix, [m (Core.Exp (Id Core.Type))])
group gcon (PatMatrix (transpose -> pss)) es = over _1 patMatrix $ unzip $ mapMaybe (aux gcon) (zip pss es)
  where
    aux gcon (ConP _ gcon' ps : pss, e)
      | gcon == gcon' = Just (ps <> pss, e)
      | otherwise = Nothing
    aux _ (p : _, _) = errorDoc $ "Invalid pattern:" <+> pPrint p
    aux _ ([], _) = error "ps must be not empty"

groupTuple :: PatMatrix -> [m (Core.Exp (Id Core.Type))] -> (PatMatrix, [m (Core.Exp (Id Core.Type))])
groupTuple (PatMatrix (transpose -> pss)) es = over _1 patMatrix $ unzip $ zipWith aux pss es
  where
    aux (TupleP _ ps : pss) e = (ps <> pss, e)
    aux (p : _) _ = errorDoc $ "Invalid pattern:" <+> pPrint p
    aux [] _ = error "ps must be not empty"

groupRecord :: (MonadReader DsEnv m, MonadIO m) => PatMatrix -> [m (Core.Exp (Id Core.Type))] -> m (PatMatrix, [m (Core.Exp (Id Core.Type))])
groupRecord (PatMatrix pss) es = over _1 patMatrix . unzip <$> zipWithM aux pss es
  where
    aux (RecordP x ps : pss) e = do
      ps' <- extendRecordP x ps
      pure (ps' <> pss, e)
    aux (p : _) _ = errorDoc $ "Invalid pattern:" <+> pPrint p
    aux [] _ = error "ps must be not empty"
    extendRecordP (Typed (Malgo.TyRecord ktsMap) pos) ps = do
      let kts = HashMap.toList ktsMap
      for kts \(key, ty) ->
        case List.lookup key ps of
          Nothing -> VarP (Typed ty pos) <$> newTemporalId "_p" ()
          Just p -> pure p
    extendRecordP _ _ = error "typeOf x must be TyRecord"
