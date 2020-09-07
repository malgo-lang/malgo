{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Grouping where

import Data.Graph
import qualified Data.Set as Set
import Language.Griff.Extension
import Language.Griff.Syntax
import Language.Malgo.Id
import Language.Malgo.Prelude
import Language.Malgo.Pretty

data BindGroup x = BindGroup
  { _scDefs :: [[(XScDef x, XId x, [XId x], Exp x)]],
    _scSigs :: [(XScSig x, XId x, Type x)],
    _dataDefs :: [(XDataDef x, XTId x, [XTId x], [(XId x, [Type x])])],
    _infixs :: [(XInfix x, Assoc, Int, XId x)],
    _forigns :: [(XForign x, XId x, Type x)]
  }

makeLenses ''BindGroup

makeBindGroup :: (Pretty a, Ord (XId x), XId x ~ Id a) => [Decl x] -> BindGroup x
makeBindGroup ds =
  BindGroup
    { _scDefs = splitScDef (makeSCC $ mapMaybe scDef ds) (mapMaybe scDef ds),
      _scSigs = mapMaybe scSig ds,
      _dataDefs = mapMaybe dataDef ds,
      _infixs = mapMaybe infixDef ds,
      _forigns = mapMaybe forign ds
    }
  where
    scDef (ScDef x f ps e) = Just (x, f, ps, e)
    scDef _ = Nothing
    scSig (ScSig x f t) = Just (x, f, t)
    scSig _ = Nothing
    dataDef (DataDef x t ps cons) = Just (x, t, ps, cons)
    dataDef _ = Nothing
    infixDef (Infix x a o n) = Just (x, a, o, n)
    infixDef _ = Nothing
    forign (Forign x n t) = Just (x, n, t)
    forign _ = Nothing
    splitScDef sccs ds = map (\ns -> mapMaybe (\n -> find (\d -> n == d ^. _2) ds) ns) sccs

adjacents :: (Pretty a, Ord (XId x), XId x ~ Id a) => (XScDef x, Id a, [Id a], Exp x) -> (Id a, Int, [Int])
adjacents (_, f, ps, e) = (f, f ^. idUniq, map (view idUniq) $ toList $ freevars e Set.\\ Set.insert f (Set.fromList ps))

makeSCC :: (Pretty a, Ord (XId x), XId x ~ Id a) => [(XScDef x, Id a, [Id a], Exp x)] -> [[Id a]]
makeSCC ds = map flattenSCC $ stronglyConnComp adjacents'
  where
    vertices = map (view _2) $ map adjacents ds
    adjacents' = map (\(l, v, vs) -> (l, v, filter (`elem` vertices) vs)) $ map adjacents ds
