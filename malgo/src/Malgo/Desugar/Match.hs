-- | パターンマッチのコンパイル
module Malgo.Desugar.Match (match, PatMatrix, patMatrix) where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Koriel.Core.Syntax
import qualified Koriel.Core.Syntax as Core
import Koriel.Core.Type
import qualified Koriel.Core.Type as Core
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Pretty
import Malgo.Desugar.DsEnv
import Malgo.Desugar.Type (dsType, unfoldType)
import Malgo.Desugar.Unboxed (dsUnboxed)
import Malgo.Prelude
import Malgo.Syntax
import Malgo.Syntax.Extension
import Malgo.TypeRep.Static
import qualified Malgo.TypeRep.Static as Malgo

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

-- consCol :: [Pat (Malgo 'Refine)] -> PatMatrix -> PatMatrix
-- consCol ps PatMatrix {..} = PatMatrix (ps : innerList)

splitCol :: PatMatrix -> (Maybe [Pat (Malgo 'Refine)], PatMatrix)
splitCol mat = (headCol mat, tailCol mat)

-- パターンマッチを分解し、switch-case相当の分岐で表現できるように変換する
match ::
  (MonadState DsEnv m, MonadIO m, MonadReader env m, MonadFail m, HasUniqSupply env) =>
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
              (VarP _ v) -> \e -> nameEnv . at v ?= scrutinee >> e
              _ -> bug $ Unreachable "All elements of heads must be VarP"
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
          Just (Malgo.TypeRep.Static.TyCon con, ts) -> (con, ts)
          _ -> bug $ Unreachable "patType must be TyApp or TyCon"
    valueConstructors <- lookupValueConstructors con ts
    -- 各コンストラクタごとにC.Caseを生成する
    cases <- for valueConstructors \(conName, Forall _ conType) -> do
      paramTypes <- traverse dsType $ fst $ splitTyArr conType
      let coreCon = Core.Con (Data $ conName ^. toText) paramTypes
      params <- traverse (newLocalId "$p") paramTypes
      let (pat', es') = group conName pat es
      Unpack coreCon params <$> match (params <> restScrutinee) pat' es' err
    unfoldedType <- unfoldType patType
    pure $ Match (Cast unfoldedType $ Core.Var scrutinee) $ NonEmpty.fromList cases
  -- パターンの先頭がすべてレコードのとき
  | all (has _RecordP) heads = do
    let patType = Malgo.typeOf $ List.head heads
    SumT [con@(Core.Con Core.Tuple ts)] <- dsType patType
    params <- traverse (newLocalId "$p") ts
    cases <- do
      (pat', es') <- groupRecord pat es
      -- NonEmpty.singleton = (:| [])
      (:| []) . Unpack con params <$> match (params <> restScrutinee) pat' es' err
    pure $ Match (Atom $ Core.Var scrutinee) cases
  -- パターンの先頭がすべてタプルのとき
  | all (has _TupleP) heads = do
    let patType = Malgo.typeOf $ List.head heads
    SumT [con@(Core.Con Core.Tuple ts)] <- dsType patType
    params <- traverse (newLocalId "$p") ts
    cases <- do
      let (pat', es') = groupTuple pat es
      -- NonEmpty.singleton = (:| [])
      (:| []) . Unpack con params <$> match (params <> restScrutinee) pat' es' err
    pure $ Match (Atom $ Core.Var scrutinee) cases
  -- パターンの先頭がすべてunboxedな値のとき
  | all (has _UnboxedP) heads = do
    let cs =
          map
            ( \case
                UnboxedP _ x -> dsUnboxed x
                _ -> bug $ Unreachable "All elements of heads must be UnboxedP"
            )
            heads
    cases <- traverse (\c -> Switch c <$> match restScrutinee tails es err) cs
    -- パターンの網羅性を保証するため、
    -- `_ -> err` を追加する
    hole <- newLocalId "$_" (Core.typeOf scrutinee)
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
partition (splitCol -> (Just heads@(VarP {} : _), PatMatrix tails)) es =
  let (heads', heads'') = span (has _VarP) heads
   in (bimap (PatMatrix . (heads' :)) (PatMatrix . (heads'' :)) $ unzip $ map (List.splitAt (length heads')) tails, List.splitAt (length heads') es)
partition (splitCol -> (Just heads@(ConP {} : _), PatMatrix tails)) es =
  let (heads', heads'') = span (has _ConP) heads
   in (bimap (PatMatrix . (heads' :)) (PatMatrix . (heads'' :)) $ unzip $ map (List.splitAt (length heads')) tails, List.splitAt (length heads') es)
partition (splitCol -> (Just heads@(TupleP {} : _), PatMatrix tails)) es =
  let (heads', heads'') = span (has _TupleP) heads
   in (bimap (PatMatrix . (heads' :)) (PatMatrix . (heads'' :)) $ unzip $ map (List.splitAt (length heads')) tails, List.splitAt (length heads') es)
partition (splitCol -> (Just heads@(RecordP {} : _), PatMatrix tails)) es =
  let (heads', heads'') = span (has _RecordP) heads
   in (bimap (PatMatrix . (heads' :)) (PatMatrix . (heads'' :)) $ unzip $ map (List.splitAt (length heads')) tails, List.splitAt (length heads') es)
partition (splitCol -> (Just heads@(UnboxedP {} : _), PatMatrix tails)) es =
  let (heads', heads'') = span (has _UnboxedP) heads
   in (bimap (PatMatrix . (heads' :)) (PatMatrix . (heads'' :)) $ unzip $ map (List.splitAt (length heads')) tails, List.splitAt (length heads') es)
partition _ _ = bug $ Unreachable "All patterns are covered"

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
    aux _ ([], _) = bug $ Unreachable "ps must be not empty"

groupTuple :: PatMatrix -> [m (Core.Exp (Id Core.Type))] -> (PatMatrix, [m (Core.Exp (Id Core.Type))])
groupTuple (PatMatrix pss) es = over _1 patMatrix $ unzip $ zipWith aux pss es
  where
    aux (TupleP _ ps : pss) e = (ps <> pss, e)
    aux (p : _) _ = errorDoc $ "Invalid pattern:" <+> pPrint p
    aux [] _ = bug $ Unreachable "ps must be not empty"

groupRecord :: (MonadReader env m, MonadIO m, HasUniqSupply env) => PatMatrix -> [m (Core.Exp (Id Core.Type))] -> m (PatMatrix, [m (Core.Exp (Id Core.Type))])
groupRecord (PatMatrix pss) es = over _1 patMatrix . unzip <$> zipWithM aux pss es
  where
    aux (RecordP x ps : pss) e = do
      ps' <- extendRecordP x $ map (first removePrefix) ps
      pure (ps' <> pss, e)
    aux (p : _) _ = errorDoc $ "Invalid pattern:" <+> pPrint p
    aux [] _ = bug $ Unreachable "ps must be not empty"
    extendRecordP (With (Malgo.TyRecord ktsMap) pos) ps = do
      let kts = Map.toList ktsMap
      for kts \(key, ty) ->
        case List.lookup key ps of
          Nothing -> VarP (With ty pos) <$> newLocalId "$_p" ()
          Just p -> pure p
    extendRecordP _ _ = bug $ Unreachable "typeOf x must be TyRecord"
