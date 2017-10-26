module Language.Malgo.Types where

import           Data.String
import           Text.Parsec     (SourcePos)
import qualified Text.Parsec.Pos

-- | ソースコードの位置情報
-- | forall a b. Info a == Info b
newtype Info = Info SourcePos

instance Eq Info where
  _ == _ = True

instance Show Info where
  show (Info x) = show x

dummyInfo :: Info
dummyInfo = Info $ Text.Parsec.Pos.newPos "<dummy>" 0 0

data Name = Name String
          | DummyName
  deriving (Show, Eq)

instance IsString Name where
  fromString = Name

-- | 中置演算子の種類を表すタグ
data Op = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Le | Ge | And | Or
  deriving (Eq, Show)

-- | Malgoの組み込みデータ型
data Type = IntTy -- ^ 32bit整数
          | FloatTy -- ^ 倍精度浮動小数点数
          | BoolTy
          | CharTy
          | StringTy
          | UnitTy
          | FunTy Type [Type]
  deriving (Eq, Show)

-- | 比較の計算量が定数時間
newtype Id = Id (Int, Name)
  deriving Show

instance IsString Id where
  fromString x = Id (0, Name x)

instance Eq Id where
  (Id (x, _)) == (Id (y, _)) = x == y
