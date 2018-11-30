{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Type
  ( TypeScheme(..)
  , TyRef
  , _tyRefId
  , _tyRef
  , newTyRef
  , readTyRef
  , writeTyRef
  , modifyTyRef
  , Type(..)
  , PrimC(..)
  , TyCon(..)
  , intType, doubleType, charType
  , boolType, stringType, arrowType
  , unitType, tupleType, arrayType
  , answerType, (-->), applyType
  , replaceType
  ) where

import           Control.Monad.IO.Class
import           Data.IORef
import qualified Data.List              as List
import           Data.Maybe
import           Data.Outputable
import           GHC.Generics           (Generic)
import           Language.Malgo.Monad
import           Language.Malgo.Pretty

data TypeScheme a = Forall [a] (Type a)
  deriving (Eq, Show, Generic)

instance Outputable a => Outputable (TypeScheme a)

instance Pretty a => Pretty (TypeScheme a) where
  pPrint (Forall xs ty) = "forall" <+> sep (map pPrint xs) <> "." <+> pPrint ty

data TyRef a = TyRef { _tyRefId :: Int, _tyRef :: IORef (Maybe (Type a)) }
  deriving Eq

instance Ord (TyRef a) where
  compare x y = compare (_tyRefId x) (_tyRefId y)

newTyRef :: MonadMalgo m => m (TyRef a)
newTyRef = TyRef <$> newUniq <*> liftIO (newIORef Nothing)
readTyRef :: MonadIO m => TyRef a -> m (Maybe (Type a))
readTyRef (TyRef _ r) = liftIO $ readIORef r
writeTyRef :: (MonadIO m, Outputable a) => TyRef a -> Type a -> m ()
writeTyRef (TyRef _ r) ty = do
  mt <- liftIO $ readIORef r
  case mt of
    Nothing -> liftIO $ writeIORef r (Just ty)
    Just ty' -> error $ show $ "rewrite(writeTyRef):" <+> ppr ty' <+> "->" <+> ppr ty
modifyTyRef
  :: MonadIO m => TyRef a -> (Maybe (Type a) -> Maybe (Type a)) -> m ()
modifyTyRef (TyRef _ r) = liftIO . modifyIORef r

instance Show (TyRef a) where
  show (TyRef i _) = "<TyRef " <> Prelude.show i <> ">"

instance Pretty (TyRef a) where
  pPrint (TyRef i _) = "<TyRef" <+> pPrint i <> ">"

instance Outputable (TyRef a) where
  pprPrec _ (TyRef i _) = "<TyRef" <+> ppr i <> ">"

data Type a = TyApp (TyCon a) [Type a]
            | TyVar a
            | TyMeta (TyRef a)
  deriving (Eq, Show, Generic)

instance Outputable a => Outputable (Type a)

instance Pretty a => Pretty (Type a) where
  pPrintPrec l d (TyApp (PrimC ArrowC) [x, y]) =
    maybeParens (d > 5) $ pPrintPrec l 6 x <+> "->" <+> pPrintPrec l 6 y
  pPrintPrec _ _ (TyApp (PrimC (TupleC n)) xs)
    | length xs == n = parens $ sep $ punctuate "," $ map pPrint xs
  pPrintPrec l d (TyApp con args) =
    maybeParens (d > 10) $ pPrint con <+> sep (map (pPrintPrec l 11) args)
  pPrintPrec _ _ (TyVar x) = pPrint x
  pPrintPrec _ _ (TyMeta x) = pPrint x

data PrimC = IntC | DoubleC | CharC | BoolC | StringC | TupleC Int | ArrowC | ArrayC | AnswerC
  deriving (Eq, Show, Generic)

instance Outputable PrimC

instance Pretty PrimC where
  pPrint IntC       = "Int"
  pPrint DoubleC    = "Double"
  pPrint CharC      = "Char"
  pPrint BoolC      = "Bool"
  pPrint StringC    = "String"
  pPrint (TupleC n) = "(" <> text (replicate n ',') <> ")"
  pPrint ArrowC     = "(->)"
  pPrint ArrayC     = "Array"
  pPrint AnswerC    = "Answer"

data TyCon a = PrimC PrimC
             | SimpleC a
  deriving (Eq, Show, Generic)

instance Outputable a => Outputable (TyCon a)

instance Pretty a => Pretty (TyCon a) where
  pPrint (PrimC c)   = pPrint c
  pPrint (SimpleC c) = pPrint c

intType :: Type a
intType = TyApp (PrimC IntC) []
doubleType :: Type a
doubleType = TyApp (PrimC DoubleC) []
charType :: Type a
charType = TyApp (PrimC CharC) []
boolType :: Type a
boolType = TyApp (PrimC BoolC) []
stringType :: Type a
stringType = TyApp (PrimC StringC) []
arrowType :: Type a -> Type a -> Type a
arrowType a b = TyApp (PrimC ArrowC) [a, b]
unitType :: Type a
unitType = TyApp (PrimC $ TupleC 0) []
tupleType :: [Type a] -> Type a
tupleType xs = TyApp (PrimC $ TupleC $ length xs) xs
arrayType :: Type a -> Type a
arrayType a = TyApp (PrimC ArrayC) [a]
answerType :: Type a
answerType = TyApp (PrimC AnswerC) []

infixr 5 -->
(-->) :: Type a -> Type a -> Type a
(-->) = arrowType

applyType :: (Eq a, MonadIO f) => ([a], Type a) -> [Type a] -> f (Type a)
applyType (ks, t) vs = replaceType (zip ks vs) t

replaceType :: (Eq a, MonadIO f) => [(a, Type a)] -> Type a -> f (Type a)
replaceType kvs (TyApp tycon ts) = TyApp tycon <$> mapM (replaceType kvs) ts
replaceType kvs (TyVar v) = return $ fromMaybe (TyVar v) $ List.lookup v kvs
replaceType kvs m@(TyMeta (TyRef _ r)) = do
  mt <- liftIO $ readIORef r
  case mt of
    Just ty -> replaceType kvs ty
    Nothing -> return m
