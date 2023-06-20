{-# LANGUAGE DeriveAnyClass #-}

module Koriel.Core.Syntax.LocalDef (
  LocalDef (..),
  HasObject (..),
  HasVariable (..),
  Obj (..),
)
where

import Control.Lens (Lens', sans, traverseOf, traversed)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import Data.String.Conversions
import Generic.Data
import Koriel.Core.Syntax.Atom
import Koriel.Core.Syntax.Common
import {-# SOURCE #-} Koriel.Core.Syntax.Expr
import Koriel.Core.Type
import Koriel.Prelude
import Koriel.Pretty

-- | Let bindings
data LocalDef a = LocalDef {_variable :: a, typ :: Type, _object :: Obj a}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Binary, ToJSON, FromJSON)

class HasObject s a | s -> a where
  object :: Lens' s a

instance HasObject (LocalDef a) (Obj a) where
  {-# INLINE object #-}
  object f (LocalDef x1 t x2) = fmap (LocalDef x1 t) (f x2)

class HasVariable s a | s -> a where
  variable :: Lens' s a

instance HasVariable (LocalDef a) a where
  {-# INLINE variable #-}
  variable f (LocalDef x1 t x2) = fmap (\x1 -> LocalDef x1 t x2) (f x1)

instance Pretty a => Pretty (LocalDef a) where
  pPrint (LocalDef v t o) = parens $ pPrint v <+> pPrint t $$ pPrint o

instance HasAtom LocalDef where
  atom = object . atom

-- | heap objects
data Obj a
  = -- | function (arity >= 1)
    Fun [a] (Expr a)
  | -- | saturated constructor (arity >= 0)
    Pack Type Con [Atom a]
  | -- | record
    Record (HashMap Text (Atom a))
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Data, Typeable)
  deriving anyclass (Binary, ToJSON, FromJSON)

instance HasType a => HasType (Obj a) where
  typeOf (Fun xs e) = map typeOf xs :-> typeOf e
  typeOf (Pack t _ _) = t
  typeOf (Record kvs) = RecordT (fmap typeOf kvs)

instance Pretty a => Pretty (Obj a) where
  pPrint (Fun xs e) = parens $ sep ["fun" <+> parens (sep $ map pPrint xs), pPrint e]
  pPrint (Pack ty c xs) = parens $ sep (["pack", pPrint ty, pPrint c] <> map pPrint xs)
  pPrint (Record kvs) =
    parens $
      sep
        [ "record"
            <+> parens
              ( sep $
                  map
                    ( \(k, v) ->
                        pPrint k
                          <+> pPrint v
                    )
                    (HashMap.toList kvs)
              )
        ]

instance HasFreeVar Obj where
  freevars (Fun as e) = foldr sans (freevars e) as
  freevars (Pack _ _ xs) = foldMap freevars xs
  freevars (Record kvs) = foldMap freevars kvs

instance HasAtom Obj where
  atom f = \case
    Fun xs e -> Fun xs <$> traverseOf atom f e
    Pack ty con xs -> Pack ty con <$> traverseOf (traversed . atom) f xs
    Record kvs -> Record <$> traverseOf (traversed . atom) f kvs
