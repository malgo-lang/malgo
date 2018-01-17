{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Language.Malgo.Utils where

import           Control.Monad.Except
import           Control.Monad.State
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

type Name = BS.ByteString

fromName :: IsString a => Name -> a
fromName = fromString . BS.unpack

instance PrettyPrint BS.ByteString where
  pretty bs = P.text (BS.unpack bs)

data MalgoError = RenameError Info P.Doc
                | TypeCheckError Info P.Doc
                | KNormalError Info P.Doc
                | BetaTransError P.Doc
                | ClosureTransError P.Doc
                | EvalError P.Doc
  deriving Show

instance PrettyPrint MalgoError where
  -- pretty (ParseError i m) =
  --   P.text "error(parse):" P.<+> pretty i P.<+> m
  pretty (RenameError i m) =
    P.text "error(rename):" P.<+> pretty i P.<+> m
  pretty (TypeCheckError i m) =
    P.text "error(typing):" P.<+> pretty i P.<+> m
  pretty (KNormalError i m) =
    P.text "error(knormal):" P.<+> pretty i P.<+> m
  pretty (BetaTransError m) =
    P.text "error(betatrans):" P.<+> m
  pretty (ClosureTransError m) =
    P.text "error(closuretrans):" P.<+> m
  pretty (EvalError m) =
    P.text "error(eval):" P.<+> m

class Env e where
  initEnv :: e

newtype MalgoT s m a = MalgoT { unMalgoT :: ExceptT MalgoError (StateT s (StateT Int m)) a }
  deriving ( Functor, Applicative
           , Monad, MonadError MalgoError, MonadState s
           , MonadIO
           )

instance MonadTrans (MalgoT s) where
  lift = MalgoT . lift . lift . lift

newUniq :: Monad m => MalgoT s m Int
newUniq = do
  c <- MalgoT . lift . lift $ get
  MalgoT . lift . lift . modify $ \e -> e + 1
  return c

doMalgoT ::
  (Env s, Monad m) =>
  MalgoT s m a -> StateT Int m (Either MalgoError a)
doMalgoT (MalgoT m) = evalStateT (runExceptT m) initEnv

runMalgoT :: (Env s, Monad m) => MalgoT s m a -> Int -> m (Either MalgoError a, s)
runMalgoT (MalgoT m) = evalStateT (runStateT (runExceptT m) initEnv)
