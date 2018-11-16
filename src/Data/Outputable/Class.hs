{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeOperators     #-}
module Data.Outputable.Class (Outputable(..)) where

import           Data.Char
import           GHC.Generics
import           Text.PrettyPrint hiding ((<>))

wrapParens :: Bool -> Doc -> Doc
wrapParens False s = s
wrapParens True x  = lparen <> x <> rparen

class Outputable a where
  pprPrec :: Int -> a -> Doc
  default pprPrec :: (Generic a, GOutputable (Rep a)) => Int -> a -> Doc
  pprPrec n x = gppr (from x) Pref n False

  ppr :: a -> Doc
  ppr = pprPrec 0

  pprList :: [a] -> Doc
  pprList =
    brackets . sep . punctuate comma . map ppr

data Type = Rec | Pref | Inf String
  deriving Eq

class GOutputable (f :: * -> *) where
  gppr :: f x -> Type -> Int -> Bool -> Doc
  isNullary :: f x -> Bool

instance GOutputable V1 where
  gppr _ _ _ _ = mempty
  isNullary = error "generic outputable (isNullary): unnecessary case"

instance GOutputable U1 where
  gppr _ _ _ _ = mempty
  isNullary _ = True

instance (GOutputable f, Datatype c) => GOutputable (M1 D c f) where
  gppr (M1 a) = gppr a
  isNullary (M1 a) = isNullary a

instance (GOutputable f, Selector c) => GOutputable (M1 S c f) where
  gppr s@(M1 a) t d p
    | selector == "" = gppr a t d p
    | otherwise = fsep [text selector <+> char '=', nest 2 $ gppr a t 0 p]
    where selector = selName s
  isNullary (M1 a) = isNullary a

instance (GOutputable f, Constructor c) => GOutputable (M1 C c f) where
  gppr c@(M1 a) _ d _ =
    case fixity of
      Prefix -> wrapParens boolParens $ text name <+>
                if t == Rec
                then nest 1 $ braces $ nest 2 $ gppr a t 11 boolParens
                else nest 2 $ gppr a t 11 boolParens
      Infix _ m -> wrapParens (d > m) $ gppr a t (m + 1) (d > m)
    where fixity = conFixity c
          boolParens = d > 10 && not (isNullary a)
          t | conIsRecord c = Rec
            | otherwise = case fixity of
                            Prefix    -> Pref
                            Infix _ _ -> Inf (conName c)
          name = checkInfix $ conName c
          checkInfix [] = []
          checkInfix (x:xs)
            | fixity == Prefix && (isAlphaNum x || x == '_') = x:xs
            | otherwise = "(" ++ (x:xs) ++ ")"
  isNullary (M1 a) = isNullary a

instance (Outputable f) => GOutputable (K1 t f) where
  gppr (K1 a) _ d _ = pprPrec d a
  isNullary _ = False

instance (GOutputable f, GOutputable g) => GOutputable (f :+: g) where
  gppr (L1 a) t d p = gppr a t d p
  gppr (R1 a) t d p = gppr a t d p
  isNullary (L1 a) = isNullary a
  isNullary (R1 a) = isNullary a

instance (GOutputable f, GOutputable g) => GOutputable (f :*: g) where
  gppr (f :*: g) Rec d p = sep $ punctuate comma [pfn, pgn]
    where pfn = gppr f Rec d p
          pgn = gppr g Rec d p

  gppr (f :*: g) t@(Inf s) d p =
    pfn <+> text s <+> pgn
    where pfn = gppr f t d p
          pgn = gppr g t d p

  gppr (f :*: g) Pref n p =
    gppr f Pref n p <+> gppr g Pref n p

  isNullary _ = False
