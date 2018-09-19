{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Malgo.IR.AST where

import           Data.Outputable
import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.Type
import           Universum

data Expr a = Var SrcSpan a
            | Literal SrcSpan Literal
            | Record SrcSpan [(a, Expr a)]
            | Variant SrcSpan a (Expr a) SType
            | Let [Bind a] (Expr a)
            | Apply (Expr a) (Expr a)
            | Case SrcSpan (Expr a) [Clause a]
            | Fn SrcSpan [(a, Maybe SType)] (Expr a)
  deriving (Eq, Show, Generic, Outputable)

{- # Function literal transformation
(fun (x:a) (y:b) (z:c) -> e:d) : a -> b -> c -> d
=> Fn SrcSpan [(x, a), (y, b), (z, c)] e
=> Fn SrcSpan [(x, a)] (Fn SrcSpan [(y, b)] (Fn SrcSpan [(z, c)] e))
-}

extendFn :: Expr a -> Expr a
extendFn (Fn ss (p:ps) e) = Fn ss [p] (extendFn $ Fn ss ps e)
extendFn e                = e

data Literal = Int Integer
             | Float Double
             | Bool Bool
             | Char Char
  deriving (Eq, Show, Generic, Outputable)

data Bind a = NonRec SrcSpan a (Maybe SType) (Expr a)
            | Rec SrcSpan a (Maybe SType) [a] (Expr a)
  deriving (Eq, Show, Generic, Outputable)

splitBinds :: [Bind a] -> [[Bind a]]
splitBinds [] = []
splitBinds (x@Rec{} : xs) =
  (x:takeWhile isRec xs) : splitBinds (dropWhile isRec xs)
splitBinds (x@NonRec{} : xs) =
  (x:takeWhile (not . isRec) xs) : splitBinds (dropWhile (not . isRec) xs)

isRec :: Bind a -> Bool
isRec Rec{} = True
isRec _     = False

{- # Rec transformation
rec f x y : a -> b -> c = e
=> Rec SrcSpan f (a -> b -> c) [x, y] e
=> Rec SrcSpan f (a -> b -> c) [] (Fn SrcSpan [(x, a)] (Fn SrcSpan [(y, b)] e))
-}

extendRec :: (HasType a, MonadReader (Env a) m, TypeRep a ~ SType) => Bind a -> m (Bind a)
extendRec b@NonRec{} = return b
extendRec b@(Rec _ _ _ [] _) = return b
extendRec (Rec ss f ty xs e) =
  Rec ss f ty [] <$> (buildFn <$> xs')
  where
    xs' = do
      tys <- mapM typeOf xs
      return $ reverse $ zip xs (map Just tys)
    buildFn = foldr (\x -> Fn ss [x]) e

data Clause a = VariantPat SrcSpan a a SType (Expr a)
              | BoolPat SrcSpan Bool (Expr a)
              | VarPat SrcSpan a (Expr a)
  deriving (Eq, Show, Generic, Outputable)

-- | トップレベル宣言
data Decl a = ScDef SrcSpan a [a] (Expr a) -- ^ 環境を持たない関数（定数）宣言
            | ScAnn SrcSpan a SType -- ^ 関数（定数）の型宣言
            | AliasDef SrcSpan SType [Text] SType -- ^ 型の別名定義
            | TypeDef SrcSpan SType [Text] SType -- ^ 新しい型の定義
  deriving (Eq, Show, Generic, Outputable)

-- | ソースコード上での型の表現
data SType = STyApp STyCon [SType]
           | STyVar Text
  deriving (Eq, Show, Generic, Outputable)

{- # SType vs Type
型検査時にLanguage.Malgo.Type.Typeへ翻訳される．
Forallは型検査の過程で自動生成される．
-}

data STyCon = SimpleC Text
            | SRecordC [(Text, SType)]
            | SVariantC [(Text, SType)]
  deriving (Eq, Show, Generic, Outputable)
