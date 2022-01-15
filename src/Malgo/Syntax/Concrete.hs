module Malgo.Syntax.Concrete where

import Koriel.Pretty (Pretty (..), (<+>))
import qualified Koriel.Pretty as P
import Malgo.Prelude
import Text.Megaparsec (SourcePos)
import qualified Text.PrettyPrint as P

{-
 - The concrete syntax is a raw representation of the program text.
-}

data Range = Range {start :: SourcePos, end :: SourcePos}
  deriving stock (Eq, Ord, Show)

data QName = QName {moduleName :: Text, name :: Text}
  deriving stock (Eq, Ord, Show)

instance Pretty QName where
  pPrint (QName m n) = P.text (toString m) <> "." <> P.text (toString n)

data List2 a = List2 a a [a]
  deriving stock (Eq, Ord, Show, Foldable, Functor, Traversable)

instance Pretty a => Pretty (List2 a) where
  pPrint (List2 x y xs) = pPrint (x : y : xs)

-- | Concrete expressions. Should represent exactly what the user wrote.
data Expr
  = -- | @foo@
    Ident Range QName
  | -- | @1#@
    UnboxedLit Range Literal
  | -- | @1@
    BoxedLit Range Literal
  | -- | before parsing operators
    RawApp Range (List2 Expr)
  | -- | @foo bar@
    App Range Expr (NonEmpty Expr)
  | -- | @foo + bar@
    OpApp Range (NonEmpty Op) (NonEmpty Expr)
  | -- | @{ Nil -> n | Cons x xs -> x }@
    Fun Range (NonEmpty Clause)
  | -- | @(1, 2)@
    Tuple Range [Expr]
  | -- | @{ x = 1, y = 2 }@
    Record Range [(Text, Expr)]
  | -- | @[1, 2]@
    List Range [Expr]
  | -- | @1 : Int@
    Ann Range Expr Type
  | -- | @1; 2@
    Seq Range (NonEmpty Stmt)
  | -- | @(1)@
    Paren Range Expr

instance Pretty Expr where
  pPrint (Ident _ qn) = pPrint qn
  pPrint (UnboxedLit _ lit) = pPrint lit
  pPrint (BoxedLit _ lit) = pPrint lit
  pPrint (RawApp _ es) = P.parens (P.hsep $ toList $ pPrint <$> es)
  pPrint (App _ e es) = P.hsep $ pPrint <$> (e : toList es)

data Op = OpSymbol Text
        | OpPlaceholder 
  deriving stock (Eq, Ord, Show)

instance Pretty Op where
  pPrint (OpSymbol s) = P.text $ toString s
  pPrint OpPlaceholder = P.text "_"

data Clause
  = -- | @Cons x xs -> x@
    Clause Range (NonEmpty Pat) Expr

instance Pretty Clause where
  pPrint (Clause _ ps e) = P.parens (P.sep $ toList $ pPrint <$> ps) <+> "->" <+> pPrint e

data Pat
  = -- | @x@, @X@
    IdentP Range QName
  | -- | @(x, y)@
    TupleP Range [Pat]
  | -- | @{ x = x, y = y }@
    RecordP Range [(Text, Pat)]
  | -- | @[x, y]@
    ListP Range [Pat]
  | -- | @1#@
    UnboxedP Range Literal
  | -- | @1@
    BoxedP Range Literal

instance Pretty Pat where
  pPrint (IdentP _ qn) = pPrint qn
  pPrint (TupleP _ ps) = P.parens $ P.hsep $ P.punctuate "," (pPrint <$> ps)
  pPrint (RecordP _ ps) = P.braces $ P.hsep $ P.punctuate "," (pPrintField <$> ps)
    where
      pPrintField (k, v) = P.text (toString k) <> " = " <> pPrint v
  pPrint (ListP _ ps) = P.brackets $ P.hsep $ P.punctuate "," (pPrint <$> ps)
  pPrint (UnboxedP _ l) = pPrint l
  pPrint (BoxedP _ l) = pPrint l

data Literal
  = -- | @1i32@
    LitInt32 Int32
  | -- | @1i64@
    LitInt64 Int64
  | -- | @1@
    LitInt Int
  | -- | @1.0f32@
    LitFloat Float
  | -- | @1.0f64@
    LitDouble Double
  | -- | @1.0@
    LitReal Double
  | -- | @"foo"@
    LitString Text
  | -- | @'a'@
    LitChar Char

instance Pretty Literal where
  pPrint (LitInt32 i) = P.int (fromIntegral i) <> P.text "i32"
  pPrint (LitInt64 i) = P.int (fromIntegral i) <> P.text "i64"
  pPrint (LitInt i) = P.int i
  pPrint (LitFloat f) = P.float f <> P.text "f32"
  pPrint (LitDouble f) = P.double f <> P.text "f64"
  pPrint (LitReal f) = P.double f
  pPrint (LitString s) = P.doubleQuotes $ P.text (toString s)
  pPrint (LitChar c) = P.quotes $ P.char c

data Type
  = -- | @Int@
    IdentT Range QName
  | -- | before parsing operators
    RawAppT Range (List2 Type)
  | -- | @List Int@
    AppT Range Type (NonEmpty Type)
  | -- | @(Int, Int)@
    TupleT Range [Type]
  | -- | @{ x : Int, y : Int }@
    RecordT Range [(Text, Type)]
  | -- | @[Int]@
    ListT Range Type
  | -- | @(Int)@
    ParenT Range Type

instance Pretty Type where
  pPrint (IdentT _ qn) = pPrint qn
  pPrint (RawAppT _ ts) = P.hsep $ toList $ pPrint <$> ts
  pPrint (AppT _ t ts) = P.hsep $ pPrint <$> (t : toList ts)
  pPrint (TupleT _ ts) = P.parens $ P.hsep $ P.punctuate "," (pPrint <$> ts)
  pPrint (RecordT _ ts) = P.braces $ P.hsep $ P.punctuate "," (pPrintField <$> ts)
    where
      pPrintField (k, v) = P.text (toString k) <> " : " <> pPrint v
  pPrint (ListT _ t) = P.brackets $ pPrint t
  pPrint (ParenT _ t) = P.parens $ pPrint t

data Stmt
  = -- | @let foo = 1@
    Let Range QName Expr
  | -- | @with foo = 1@
    With Range (Maybe QName) Expr
  | -- | @1@
    NoBind Range Expr