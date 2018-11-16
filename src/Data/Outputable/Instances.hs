{-# LANGUAGE OverloadedStrings #-}
module Data.Outputable.Instances where

import           Control.Applicative            (ZipList)
import           Data.Char                      (isAlphaNum)
import           Data.Complex                   (Complex)
import           Data.Functor.Identity          (Identity)
import           Data.Int                       (Int16, Int32, Int64, Int8)
import           Data.List.NonEmpty             (NonEmpty)
import qualified Data.Map                       as Map
import           Data.Monoid                    (All, Any, Dual, First, Last,
                                                 Product, Sum)
import           Data.Outputable.Class
import qualified Data.Semigroup                 as S (First, Last, Max, Min,
                                                      Option, WrappedMonoid)
import           Data.Text                      (Text, unpack)
import qualified Data.Text.Lazy                 as TL (Text, unpack)
import           Data.Version                   (Version)
import           Data.Void                      (Void)
import           Data.Word                      (Word16, Word32, Word64, Word8)
import           GHC.Generics
import           Numeric.Natural                (Natural)
import           System.Exit                    (ExitCode)
import           Text.PrettyPrint.HughesPJClass

viaPretty :: Pretty a => Int -> a -> Doc
viaPretty _ = pPrint

viaPrettyNum :: (Ord a, Num a, Pretty a) => Int -> a -> Doc
viaPrettyNum n x
  | n /= 0 && x < 0 = parens $ pPrint x
  | otherwise = pPrint x

viaShow _ x = text $ show x
viaShowNum n x
  | n /= 0 && x < 0 = parens $ viaShow n x
  | otherwise = viaShow n x

instance Outputable Double where pprPrec = viaPrettyNum
instance Outputable Float where pprPrec = viaPrettyNum
instance Outputable Int where pprPrec = viaPrettyNum
instance Outputable Int8 where pprPrec = viaShowNum
instance Outputable Int16 where pprPrec = viaShowNum
instance Outputable Int32 where pprPrec = viaShowNum
instance Outputable Int64 where pprPrec = viaShowNum
instance Outputable Integer where pprPrec = viaPrettyNum
instance Outputable Natural where pprPrec = viaShow
instance Outputable Word where pprPrec = viaShow
instance Outputable Word8 where pprPrec = viaShow
instance Outputable Word16 where pprPrec = viaShow
instance Outputable Word32 where pprPrec = viaShow
instance Outputable Word64 where pprPrec = viaShow
instance Outputable TL.Text where pprPrec n x = doubleQuotes $ text $ TL.unpack x
instance Outputable Text where pprPrec n x = doubleQuotes $ text $ unpack x
instance Outputable a => Outputable [a] where pprPrec _ = pprList

instance Outputable Char where
  pprPrec _ x = quotes $ char x
  pprList = pPrint

instance Outputable Bool
instance Outputable Ordering
instance Outputable ()
instance Outputable DecidedStrictness
instance Outputable SourceStrictness
instance Outputable SourceUnpackedness
instance Outputable Associativity
instance Outputable Fixity
instance Outputable Any
instance Outputable All
instance Outputable ExitCode
instance Outputable Version
instance Outputable Void
instance Outputable a => Outputable (Maybe a)
instance Outputable p => Outputable (Par1 p)
instance Outputable a => Outputable (NonEmpty a)
instance Outputable a => Outputable (Product a)
instance Outputable a => Outputable (Sum a)
instance Outputable a => Outputable (Dual a)
instance Outputable a => Outputable (Last a)
instance Outputable a => Outputable (First a)
instance Outputable a => Outputable (Identity a)
instance Outputable a => Outputable (ZipList a)
instance Outputable a => Outputable (S.Option a)
instance Outputable m => Outputable (S.WrappedMonoid m)
instance Outputable a => Outputable (S.Last a)
instance Outputable a => Outputable (S.First a)
instance Outputable a => Outputable (S.Max a)
instance Outputable a => Outputable (S.Min a)
instance Outputable a => Outputable (Complex a)
instance (Outputable a, Outputable b) => Outputable (Either a b)
instance (Outputable a, Outputable b) => Outputable (a, b) where
  pprPrec _ (a, b) = parens $ fsep $ punctuate comma [ppr a, ppr b]
instance (Outputable a, Outputable b, Outputable c) => Outputable (a, b, c) where
  pprPrec _ (a, b, c) = parens $ fsep $ punctuate comma [ppr a, ppr b, ppr c]

instance (Outputable k, Outputable v) => Outputable (Map.Map k v) where
  pprPrec _ m = "fromList" <+> ppr (Map.toList m)
