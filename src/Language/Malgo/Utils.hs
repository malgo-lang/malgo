{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Language.Malgo.Utils where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8      as BS
import           Data.String
import qualified Text.PrettyPrint           as P

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

newtype Info =
  Info (Source, Line, Column)
  deriving (Show, Read)

instance Eq Info where
  _ == _ = True

instance PrettyPrint Info where
  pretty (Info x) = P.text . show $ x

dummyInfo :: Info
dummyInfo = Info ("<dummy>", 0, 0)

type Name = BS.ByteString

fromName :: IsString a => Name -> a
fromName = fromString . BS.unpack

instance PrettyPrint BS.ByteString where
  pretty bs = P.text (BS.unpack bs)

data MalgoError
  = RenameError Info
                P.Doc
  | TypeCheckError Info
                   P.Doc
  | KNormalError Info
                 P.Doc
  | BetaTransError P.Doc
  | ClosureTransError P.Doc
  | EvalError P.Doc
  deriving (Show)

instance PrettyPrint MalgoError where
  pretty (RenameError i m) = P.text "error(rename):" P.<+> pretty i P.<+> m
  pretty (TypeCheckError i m) = P.text "error(typing):" P.<+> pretty i P.<+> m
  pretty (KNormalError i m) = P.text "error(knormal):" P.<+> pretty i P.<+> m
  pretty (BetaTransError m) = P.text "error(betatrans):" P.<+> m
  pretty (ClosureTransError m) = P.text "error(closuretrans):" P.<+> m
  pretty (EvalError m) = P.text "error(eval):" P.<+> m

class Env e where
  initEnv :: e

instance Env () where
  initEnv = ()

data Opt = Opt
  { _srcName         :: String
  , _dumpParsed      :: Bool
  , _dumpRenamed     :: Bool
  , _dumpTyped       :: Bool
  , _dumpHIR         :: Bool
  , _dumpBeta        :: Bool
  , _dumpFlatten     :: Bool
  , _dumpClosure     :: Bool
  , _compileOnly     :: Bool
  , _notRemoveUnused :: Bool
  , _dumpLLVM        :: Bool
  } deriving (Eq, Show)

newtype MalgoT s m a = MalgoT
  { unMalgoT :: StateT s (StateT Int m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState s
             , MonadIO
             )

instance MonadTrans (MalgoT s) where
  lift = MalgoT . lift . lift

newUniq :: Monad m => MalgoT s m Int
newUniq = do
  c <- MalgoT . lift $ get
  setUniq (c + 1)
  return c

setUniq :: Monad m => Int -> MalgoT s m ()
setUniq i = MalgoT . lift $ modify $ \_ -> i

runMalgoT :: (Env s, Monad m) => MalgoT s m a -> Int -> m (a, Int)
runMalgoT (MalgoT m) = runStateT (evalStateT m initEnv)

runMalgo :: Env s => MalgoT s Identity a -> Int -> (a, Int)
runMalgo m i = runIdentity $ runMalgoT m i
