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

instance PrettyPrint MalgoError
  -- pretty (ParseError i m) =
  --   P.text "error(parse):" P.<+> pretty i P.<+> m
                                                     where
    pretty (RenameError i m) = P.text "error(rename):" P.<+> pretty i P.<+> m
    pretty (TypeCheckError i m) = P.text "error(typing):" P.<+> pretty i P.<+> m
    pretty (KNormalError i m) = P.text "error(knormal):" P.<+> pretty i P.<+> m
    pretty (BetaTransError m) = P.text "error(betatrans):" P.<+> m
    pretty (ClosureTransError m) = P.text "error(closuretrans):" P.<+> m
    pretty (EvalError m) = P.text "error(eval):" P.<+> m

class Env e where
    initEnv :: Int -> e
    updateUniq :: e -> Int -> e
    getUniq :: e -> Int

instance Env Int where
  initEnv x = x
  updateUniq _ x = x
  getUniq x = x

data Opt = Opt { _srcName         :: String
               , _dumpParsed      :: Bool
               , _dumpRenamed     :: Bool
               , _dumpTyped       :: Bool
               , _dumpHIR         :: Bool
               , _dumpBeta        :: Bool
               , _dumpFlatten     :: Bool
               , _dumpClosure     :: Bool
               , _compileOnly     :: Bool
               , _notRemoveUnused :: Bool
               , _debug           :: Bool
               }
  deriving (Eq, Show)

dummyOpt :: Opt
dummyOpt = Opt "" False False False False False False False False False False

newtype MalgoT s m a = MalgoT
    { unMalgoT :: ExceptT MalgoError (StateT s m) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadError MalgoError
               , MonadState s
               , MonadIO
               )

instance MonadTrans (MalgoT s) where
    lift = MalgoT . lift . lift

newUniq :: (Env s, Monad m) => MalgoT s m Int
newUniq = do
    c <- gets getUniq
    setUniq (c + 1)
    return c

setUniq :: (Env s, Monad m) => Int -> MalgoT s m ()
setUniq i =
  modify $ \e -> updateUniq e i

runMalgoT :: Env s => MalgoT s m a -> m (Either MalgoError a, s)
runMalgoT (MalgoT m) = runStateT (runExceptT m) (initEnv 0)

runMalgo :: Env s => MalgoT s Identity a -> (Either MalgoError a, s)
runMalgo = runIdentity . runMalgoT
