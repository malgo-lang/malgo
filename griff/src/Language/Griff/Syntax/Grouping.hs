{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | トップレベルの宣言を、依存関係に基づいて分割する
module Language.Griff.Syntax.Grouping where

import Data.Graph
import qualified Data.Set as Set
import Koriel.Id
import Koriel.Pretty
import Language.Griff.Prelude
import Language.Griff.Syntax
import Language.Griff.Syntax.Extension

data BindGroup x = BindGroup
  { _scDefs :: [[ScDef x]],
    _scSigs :: [ScSig x],
    _dataDefs :: [DataDef x],
    _infixs :: [Infix x],
    _foreigns :: [Foreign x],
    _imports :: [Import x]
  }

type ScDef x = (XScDef x, XId x, [XId x], Exp x)

type ScSig x = (XScSig x, XId x, Type x)

type DataDef x = (XDataDef x, XTId x, [XTId x], [(XId x, [Type x])])

type Infix x = (XInfix x, Assoc, Int, XId x)

type Foreign x = (XForeign x, XId x, Type x)

type Import x = (XImport x, ModuleName)

makeLenses ''BindGroup

deriving stock instance (ForallDeclX Eq x, Eq (XId x), Eq (XTId x)) => Eq (BindGroup x)

deriving stock instance (ForallDeclX Show x, Show (XId x), Show (XTId x)) => Show (BindGroup x)

instance (Pretty (XId x), Pretty (XTId x)) => Pretty (BindGroup x) where
  pPrint BindGroup {_scDefs, _scSigs, _dataDefs, _infixs, _foreigns} =
    sep $
      punctuate ";" $
        map prettyDataDef _dataDefs
          <> map prettyInfix _infixs
          <> map prettyForeign _foreigns
          <> map prettyScSig _scSigs
          <> concatMap (map prettyScDef) _scDefs
    where
      prettyDataDef (_, d, xs, cs) =
        sep
          [ "data" <+> pPrint d <+> sep (map pPrint xs) <+> "=",
            nest 2 $ foldl1 (\a b -> sep [a, "|" <+> b]) $ map pprConDef cs
          ]
      pprConDef (con, ts) = pPrint con <+> sep (map (pPrintPrec prettyNormal 12) ts)
      prettyInfix (_, a, o, x) = "infix" <> pPrint a <+> pPrint o <+> pPrint x
      prettyForeign (_, x, t) = "foreign import" <+> pPrint x <+> "::" <+> pPrint t
      prettyScSig (_, f, t) = pPrint f <+> "::" <+> pPrint t
      prettyScDef (_, f, xs, e) =
        sep [pPrint f <+> sep (map pPrint xs) <+> "=", nest 2 $ pPrint e]

makeBindGroup :: (Pretty a, Ord (XId x), XId x ~ Id a) => [Decl x] -> BindGroup x
makeBindGroup ds =
  BindGroup
    { _scDefs = splitScDef (makeSCC $ mapMaybe scDef ds) (mapMaybe scDef ds),
      _scSigs = mapMaybe scSig ds,
      _dataDefs = mapMaybe dataDef ds,
      _infixs = mapMaybe infixDef ds,
      _foreigns = mapMaybe foreignDef ds,
      _imports = mapMaybe importDef ds
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
    foreignDef (Foreign x n t) = Just (x, n, t)
    foreignDef _ = Nothing
    importDef (Import x m) = Just (x, m)
    importDef _ = Nothing
    splitScDef sccs ds = map (mapMaybe (\n -> find (\d -> n == d ^. _2) ds)) sccs

adjacents ::
  (Pretty a, Ord (XId x), XId x ~ Id a) =>
  (XScDef x, Id a, [Id a], Exp x) ->
  (Id a, Int, [Int])
adjacents (_, f, ps, e) =
  (f, f ^. idUniq, map (view idUniq) $ toList $ freevars e Set.\\ Set.insert f (Set.fromList ps))

makeSCC :: (Pretty a, Ord (XId x), XId x ~ Id a) => [(XScDef x, Id a, [Id a], Exp x)] -> [[Id a]]
makeSCC ds = map flattenSCC $ stronglyConnComp adjacents'
  where
    vertices = map (view _2 . adjacents) ds
    adjacents' = map ((\(l, v, vs) -> (l, v, filter (`elem` vertices) vs)) . adjacents) ds