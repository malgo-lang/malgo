module Language.Malgo.Types where

import           Data.String
import           Language.Malgo.PrettyPrint
import           Text.Parsec                (SourcePos)
import qualified Text.Parsec.Pos
import qualified Text.PrettyPrint           as P

-- | ソースコードの位置情報
-- | forall a b. Info a == Info b
newtype Info = Info SourcePos

instance Eq Info where
  _ == _ = True

instance Show Info where
  show (Info x) = show x

instance PrettyPrint Info where
  pretty = P.text . show

dummyInfo :: Info
dummyInfo = Info $ Text.Parsec.Pos.newPos "<dummy>" 0 0

data Name = Name String
          | DummyName
  deriving (Show, Eq)

instance IsString Name where
  fromString = Name

fromName :: Name -> String
fromName (Name x)  = x
fromName DummyName = "<dummy>"

instance PrettyPrint Name where
  pretty = P.text . fromName

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
  pretty Neq = P.text "/="
  pretty Lt  = P.text "<"
  pretty Gt  = P.text ">"
  pretty Le  = P.text "<="
  pretty Ge  = P.text ">="
  pretty And = P.text "&&"
  pretty Or  = P.text "||"

-- | Malgoの組み込みデータ型
data Type = IntTy -- ^ 32bit整数
          | FloatTy -- ^ 倍精度浮動小数点数
          | BoolTy
          | CharTy
          | StringTy
          | UnitTy
          | FunTy Type [Type]
  deriving (Eq, Show)

instance PrettyPrint Type where
  pretty IntTy    = P.text "Int"
  pretty FloatTy  = P.text "Float"
  pretty BoolTy   = P.text "Bool"
  pretty CharTy   = P.text "Char"
  pretty StringTy = P.text "String"
  pretty UnitTy   = P.text "Unit"
  pretty (FunTy retTy argTys) =
    case argTys of
      [] -> error $ "pretty print error: the length of argTys must be 1 or more. " ++ show (FunTy retTy argTys)
      [a] -> pretty a P.<+> P.text "->" P.<+> pretty retTy
      _ -> P.cat $ P.punctuate (P.text " * ") $ map pretty argTys

-- | 比較の計算量が定数時間
data Id = Id (Int, Name)
        | Raw Name
  deriving Show

instance IsString Id where
  fromString x = Id (0, Name x)

instance Eq Id where
  (Id (x, _)) == (Id (y, _)) = x == y
  (Raw n1) == (Raw n2) = n1 == n2
  _ == _ = False

instance PrettyPrint Id where
  pretty (Id (i, n)) = pretty n P.<> P.text "_" P.<> P.int i
  pretty (Raw n)     = pretty n
