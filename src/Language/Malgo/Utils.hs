{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Language.Malgo.Utils where

import Language.Malgo.Prelude
import qualified Text.PrettyPrint           as P

type Name = Text

fromName :: StringConv Name a => Name -> a
fromName = toS

data MalgoError
  = RenameError Info P.Doc
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

newtype MalgoT s m a = MalgoT
  { unMalgoT :: ExceptT MalgoError (StateT s (StateT Int m)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError MalgoError
             , MonadState s
             , MonadIO
             )

instance MonadTrans (MalgoT s) where
  lift = MalgoT . lift . lift . lift

newUniq :: Monad m => MalgoT s m Int
newUniq = do
  c <- MalgoT $ lift $ lift get
  setUniq (c + 1)
  return c

setUniq :: Monad m => Int -> MalgoT s m ()
setUniq i = MalgoT $ lift $ lift $ modify $ const i

runMalgoT :: (Env s, Monad m) => MalgoT s m a -> Int -> m (Either MalgoError a, Int)
runMalgoT (MalgoT m) i = runStateT (evalStateT (runExceptT m) initEnv) i

type Malgo s a = MalgoT s Identity a

runMalgo :: Env s => Malgo s a -> Int -> (Either MalgoError a, Int)
runMalgo m i = runIdentity $ runMalgoT m i
