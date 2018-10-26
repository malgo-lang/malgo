{-# LANGUAGE DeriveGeneric #-}
module Language.Malgo.Type where

import qualified Data.List             as List
import           Data.Outputable
import           Language.Malgo.Pretty
import           Prelude               (show)
import           Universum             hiding (Type)

data TypeScheme a = Forall [a] (Type a)
  deriving (Eq, Show, Generic)

instance Outputable a => Outputable (TypeScheme a)

instance Pretty a => Pretty (TypeScheme a) where
  pPrint (Forall xs ty) = "forall" <+> sep (map pPrint xs) <> "." <+> pPrint ty

newtype TyRef a = TyRef (IORef (Maybe (Type a)))
  deriving Eq

readTyRef :: MonadIO m => TyRef a -> m (Maybe (Type a))
readTyRef (TyRef r) = readIORef r
writeTyRef :: MonadIO m => TyRef a -> Type a -> m ()
writeTyRef (TyRef r) = writeIORef r . Just
modifyTyRef :: MonadIO m => TyRef a -> (Maybe (Type a) -> Maybe (Type a)) -> m ()
modifyTyRef (TyRef r) = modifyIORef r

instance Show (TyRef a) where
  show _ = "<TyRef>"

instance Pretty (TyRef a) where
  pPrint _ = "<TyRef>"

instance Outputable (TyRef a) where
  pprPrec _ _ = "<TyRef>"

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

data PrimC = IntC | DoubleC | CharC | BoolC | StringC | TupleC Int | ArrowC | ArrayC
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
tupleType xs = TyApp (PrimC $ TupleC (length xs)) xs
arrayType :: Type a -> Type a
arrayType a = TyApp (PrimC ArrayC) [a]

infixr 5 -->
(-->) :: Type a -> Type a -> Type a
(-->) = arrowType

applyType :: (Eq a, MonadIO f) => ([a], Type a) -> [a] -> f (Type a)
applyType (ks, t) vs = replaceType (zip ks vs) t
  where
    replaceType kvs (TyApp tycon ts) = TyApp tycon <$> mapM (replaceType kvs) ts
    replaceType kvs (TyVar v) = return $ TyVar (fromMaybe v (List.lookup v kvs))
    replaceType kvs m@(TyMeta (TyRef r)) = do
      mt <- readIORef r
      case mt of
        Just ty -> do
          ty' <- replaceType kvs ty
          writeIORef r $ Just ty'
          return m
        Nothing -> return m
