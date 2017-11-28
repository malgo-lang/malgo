{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Language.Malgo.Utils where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import qualified Data.ByteString.Char8  as BS
import           Data.String
import qualified Text.PrettyPrint       as P

class PrettyPrint a where
  pretty :: a -> P.Doc

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Either a b) where
  pretty (Left a)  = P.text "Left" P.$+$ P.nest 1 (pretty a)
  pretty (Right a) = P.text "Right" P.$+$ P.nest 1 (pretty a)

instance PrettyPrint String where
  pretty = P.text

-- instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (a, b) where
--   pretty (a, b) = pretty a P.$+$ pretty b

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

-- | 比較の計算量が定数時間
data Id = Id (Int, String)
        | Raw String
  deriving Show

instance IsString Id where
  fromString x = Id (0, x)

instance Eq Id where
  (Id (x, _)) == (Id (y, _)) = x == y
  (Raw n1) == (Raw n2) = n1 == n2
  _ == _ = False

instance PrettyPrint Id where
  pretty (Id (i, n)) = pretty n P.<> P.text "_" P.<> P.int i
  pretty (Raw n)     = pretty n

fromId :: IsString a => Id -> a
fromId (Id (x, n)) = fromString $ n ++ '.' : show x
fromId (Raw n)     = fromString n

newtype Name = Name BS.ByteString
  deriving (Show, Eq, Ord)

instance IsString Name where
  fromString x = Name (fromString x)

instance PrettyPrint Name where
  pretty = fromName

fromName :: IsString a => Name -> a
fromName (Name s) = fromString (BS.unpack s)

data MalgoError = RenameError P.Doc
                | TypingError P.Doc
  deriving (Show, Eq)

instance PrettyPrint MalgoError where
  pretty (RenameError s) = P.text "error(rename):" P.<+> s
  pretty (TypingError s) = P.text "error(typing):" P.<+> s

newtype MalgoT s m a = MalgoT { unMalgoT :: ExceptT MalgoError (StateT s m) a }
  deriving ( Functor, Applicative
           , Monad, MonadError MalgoError, MonadState s
           , MonadIO
           )

instance MonadTrans (MalgoT s) where
  lift = MalgoT . lift . lift

runMalgoT :: MalgoT s m a -> s -> m (Either MalgoError a, s)
runMalgoT (MalgoT m) = runStateT (runExceptT m)

type Malgo s a = MalgoT s Identity a

runMalgo :: Malgo s a -> s -> (Either MalgoError a, s)
runMalgo m s = runIdentity (runMalgoT m s)
