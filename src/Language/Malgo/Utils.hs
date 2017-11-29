{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Malgo.Utils where

import qualified Data.ByteString.Char8 as BS
import           Data.String
import qualified Text.PrettyPrint      as P

class PrettyPrint a where
  pretty :: a -> P.Doc

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Either a b) where
  pretty (Left a)  = P.text "Left" P.$+$ P.nest 1 (pretty a)
  pretty (Right a) = P.text "Right" P.$+$ P.nest 1 (pretty a)

instance PrettyPrint String where
  pretty = P.text

-- instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (a, b) where
--   pretty (a, _) = pretty a

instance PrettyPrint Int where
  pretty = P.int

-- | ソースコードの位置情報
-- | forall a b. Info a == Info b
type Line = Int
type Column = Int
type Source = String
newtype Info = Info (Source, Line, Column)

instance Eq Info where
  _ == _ = True

instance Show Info where
  show (Info x) = show x

instance PrettyPrint Info where
  pretty = P.text . show

dummyInfo :: Info
dummyInfo = Info ("<dummy>", 0, 0)

-- data Name = Name String
--           | DummyName
--   deriving (Show, Eq)

-- instance IsString Name where fromString = Name

-- fromName :: IsString a => Name -> a
-- fromName (Name x)  = fromString x
-- fromName DummyName = fromString "<dummy>"

-- instance PrettyPrint Name where
--   pretty = P.text . fromName

-- | 中置演算子の種類を表すタグ
data Op = Add | Sub | Mul | Div
        | Mod
        | Eq | Neq
        | Lt | Gt | Le | Ge
        | And | Or
  deriving (Eq, Show)

instance PrettyPrint Op where
  pretty Add = P.text "+"
  pretty Sub = P.text "-"
  pretty Mul = P.text "*"
  pretty Div = P.text "/"
  pretty Mod = P.text "%"
  pretty Eq  = P.text "=="
  pretty Neq = P.text "<>"
  pretty Lt  = P.text "<"
  pretty Gt  = P.text ">"
  pretty Le  = P.text "<="
  pretty Ge  = P.text ">="
  pretty And = P.text "&&"
  pretty Or  = P.text "||"

type Name = BS.ByteString

fromName :: IsString a => Name -> a
fromName = fromString . BS.unpack

-- | 比較の計算量が定数時間
data Id = Id (Int, Name)
        | Raw Name
  deriving Show

instance IsString Id where
  fromString x = Id (0, (fromString x))

instance Eq Id where
  (Id (x, _)) == (Id (y, _)) = x == y
  (Raw n1) == (Raw n2) = n1 == n2
  _ == _ = False

instance PrettyPrint Id where
  pretty (Id (i, n)) = pretty n P.<> P.text "_" P.<> P.int i
  pretty (Raw n)     = pretty n

instance PrettyPrint BS.ByteString where
  pretty bs = P.text (BS.unpack bs)

fromId :: IsString a => Id -> a
fromId (Id (x, n)) = fromString $ fromName n ++ '.' : show x
fromId (Raw n)     = fromString (fromName n)
