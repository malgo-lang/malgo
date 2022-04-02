-- |
-- imperative version of Core
module Koriel.Core.Imp where

import Control.Lens (Lens', Traversal', lens, sans, traverseOf, traversed, view, _2)
import qualified Data.HashSet as HashSet
import Koriel.Core.Op
import Koriel.Core.Type
import Koriel.Id
import Koriel.Prelude
import Koriel.Pretty

class HasFreeVar f where
  -- | free variables
  freevars :: (Eq a, Hashable a) => f a -> HashSet a

-- | toplevel function definitions
data Program a = Program
  { _moduleName :: ModuleName,
    _topVars :: [(a, Block a)],
    _topFuncs :: [(a, ([a], Block a))],
    _extFuncs :: [(Text, Type)]
  }
  deriving stock (Eq, Show, Functor)

moduleName :: Lens' (Program a) ModuleName
moduleName = lens _moduleName (\p x -> p {_moduleName = x})

topVars :: Lens' (Program a) [(a, Block a)]
topVars = lens _topVars (\p x -> p {_topVars = x})

topFuncs :: Lens' (Program a) [(a, ([a], Block a))]
topFuncs = lens _topFuncs (\p x -> p {_topFuncs = x})

extFuncs :: Lens' (Program a) [(Text, Type)]
extFuncs = lens _extFuncs (\p x -> p {_extFuncs = x})

instance Pretty a => Pretty (Program a) where
  pPrint Program {..} =
    vcat $
      concat
        [ ["variables:"],
          map (\(v, e) -> parens $ sep ["define", pPrint v, pPrint e]) _topVars,
          ["functions:"],
          map (\(f, (ps, e)) -> parens $ sep [sep ["define", pPrint f, parens (sep $ map pPrint ps)], pPrint e]) _topFuncs,
          ["externals:"],
          map (\(f, t) -> parens $ sep ["extern", pPrint f, pPrint t]) _extFuncs
        ]

-- | unboxed values
data Unboxed
  = Int32 Integer
  | Int64 Integer
  | Float Float
  | Double Double
  | Char Char
  | String Text
  | Bool Bool
  deriving stock (Eq, Ord, Show)

instance HasType Unboxed where
  typeOf Int32 {} = Int32T
  typeOf Int64 {} = Int64T
  typeOf Float {} = FloatT
  typeOf Double {} = DoubleT
  typeOf Char {} = CharT
  typeOf String {} = StringT
  typeOf Bool {} = BoolT

instance Pretty Unboxed where
  pPrint (Int32 x) = pPrint x
  pPrint (Int64 x) = pPrint x
  pPrint (Float x) = pPrint x
  pPrint (Double x) = pPrint x
  pPrint (Char x) = quotes (pPrint x)
  pPrint (String x) = doubleQuotes (pPrint x)
  pPrint (Bool True) = "True#"
  pPrint (Bool False) = "False#"

-- | atoms
data Atom a
  = -- | variable
    Var a
  | -- | literal of unboxed values
    Unboxed Unboxed
  deriving stock (Eq, Show, Functor, Foldable)

instance HasType a => HasType (Atom a) where
  typeOf (Var x) = typeOf x
  typeOf (Unboxed x) = typeOf x

instance Pretty a => Pretty (Atom a) where
  pPrint (Var x) = pPrint x
  pPrint (Unboxed x) = pPrint x

instance HasFreeVar Atom where
  freevars (Var x) = one x
  freevars Unboxed {} = mempty

class HasAtom f where
  atom :: Traversal' (f a) (Atom a)

instance HasAtom Atom where
  atom = identity

-- | expressions
data Exp a
  = -- | atom (variables and literals)
    Atom (Atom a)
  | -- | application of closure
    Call (Atom a) [Atom a]
  | -- | application of function (not closure)
    CallDirect a [Atom a]
  | -- | application of llvm function
    RawCall Text Type [Atom a]
  | -- | binary operation
    BinOp Op (Atom a) (Atom a)
  | -- | type casting
    Cast Type (Atom a)
  | -- | raise an internal error
    Error Type
  deriving stock (Eq, Show, Functor, Foldable)

instance HasType a => HasType (Exp a) where
  typeOf (Atom x) = typeOf x
  typeOf (Call f xs) = case typeOf f of
    ps :-> r -> go ps (map typeOf xs) r
    _ -> errorDoc $ "Invalid type:" <+> quotes (pPrint $ typeOf f)
    where
      go [] [] v = v
      go (p : ps) (x : xs) v = replaceOf tyVar p x (go ps xs v)
      go _ _ _ = error "length ps == length xs"
  typeOf (CallDirect f xs) = case typeOf f of
    ps :-> r -> go ps (map typeOf xs) r
    _ -> error "typeOf f must be ps :-> r"
    where
      go [] [] v = v
      go (p : ps) (x : xs) v = replaceOf tyVar p x (go ps xs v)
      go _ _ _ = error "length ps == length xs"
  typeOf (RawCall _ t xs) = case t of
    ps :-> r -> go ps (map typeOf xs) r
    _ -> error "t must be ps :-> r"
    where
      go [] [] v = v
      go (p : ps) (x : xs) v = replaceOf tyVar p x (go ps xs v)
      go _ _ _ = error "length ps == length xs"
  typeOf (BinOp o x _) = case o of
    Add -> typeOf x
    Sub -> typeOf x
    Mul -> typeOf x
    Div -> typeOf x
    Mod -> typeOf x
    FAdd -> typeOf x
    FSub -> typeOf x
    FMul -> typeOf x
    FDiv -> typeOf x
    Eq -> boolT
    Neq -> boolT
    Lt -> boolT
    Gt -> boolT
    Le -> boolT
    Ge -> boolT
    And -> boolT
    Or -> boolT
    where
      boolT = BoolT
  typeOf (Cast ty _) = ty
  typeOf (Error t) = t

instance Pretty a => Pretty (Exp a) where
  pPrint (Atom x) = pPrint x
  pPrint (Call f xs) = parens $ pPrint f <+> sep (map pPrint xs)
  pPrint (CallDirect f xs) = parens $ "direct" <+> pPrint f <+> sep (map pPrint xs)
  pPrint (RawCall p t xs) = parens $ "raw" <+> pPrint p <+> pPrint t <+> sep (map pPrint xs)
  pPrint (BinOp o x y) = parens $ pPrint o <+> pPrint x <+> pPrint y
  pPrint (Cast ty x) = parens $ "cast" <+> pPrint ty <+> pPrint x
  pPrint (Error _) = "ERROR"

instance HasFreeVar Exp where
  freevars (Atom x) = freevars x
  freevars (Call f xs) = freevars f <> foldMap freevars xs
  freevars (CallDirect _ xs) = foldMap freevars xs
  freevars (RawCall _ _ xs) = foldMap freevars xs
  freevars (BinOp _ x y) = freevars x <> freevars y
  freevars (Cast _ x) = freevars x
  freevars (Error _) = mempty

instance HasAtom Exp where
  atom f = \case
    Atom x -> Atom <$> f x
    Call x xs -> Call <$> f x <*> traverse f xs
    CallDirect x xs -> CallDirect x <$> traverse f xs
    RawCall p t xs -> RawCall p t <$> traverse f xs
    BinOp o x y -> BinOp o <$> f x <*> f y
    Cast ty x -> Cast ty <$> f x
    Error t -> pure (Error t)

-- | alternatives
data Case a
  = -- | constructor pattern
    Unpack Con [a] (Block a)
  | -- | unboxed value pattern
    Switch Unboxed (Block a)
  | -- | variable pattern
    Bind a (Block a)
  deriving stock (Eq, Show, Functor, Foldable)

instance HasType a => HasType (Case a) where
  typeOf (Unpack _ _ e) = typeOf e
  typeOf (Switch _ e) = typeOf e
  typeOf (Bind _ e) = typeOf e

instance Pretty a => Pretty (Case a) where
  pPrint (Unpack c xs e) =
    parens $ sep ["unpack" <+> parens (pPrint c <+> sep (map pPrint xs)), pPrint e]
  pPrint (Switch u e) = parens $ sep ["switch" <+> pPrint u, pPrint e]
  pPrint (Bind x e) = parens $ sep ["bind" <+> pPrint x, pPrint e]

instance HasFreeVar Case where
  freevars (Unpack _ xs e) = foldr sans (freevars e) xs
  freevars (Switch _ e) = freevars e
  freevars (Bind x e) = sans x $ freevars e

instance HasAtom Case where
  atom f = \case
    Unpack con xs e -> Unpack con xs <$> traverseOf atom f e
    Switch u e -> Switch u <$> traverseOf atom f e
    Bind a e -> Bind a <$> traverseOf atom f e

-- | heap objects
data Obj a
  = -- | function (arity >= 1)
    Fun [a] (Block a)
  | -- | saturated constructor (arity >= 0)
    Pack [Con] Con [Atom a]
  deriving stock (Eq, Show, Functor, Foldable)

instance HasType a => HasType (Obj a) where
  typeOf (Fun xs e) = map typeOf xs :-> typeOf e
  typeOf (Pack cs _ _) = SumT cs

instance Pretty a => Pretty (Obj a) where
  pPrint (Fun xs e) = parens $ sep ["fun" <+> parens (sep $ map pPrint xs), pPrint e]
  pPrint (Pack cs c xs) = parens $ sep (["pack", pPrint cs, pPrint c] <> map pPrint xs)

instance HasFreeVar Obj where
  freevars (Fun as e) = foldr sans (freevars e) as
  freevars (Pack _ _ xs) = foldMap freevars xs

instance HasAtom Obj where
  atom f = \case
    Fun xs e -> Fun xs <$> traverseOf atom f e
    Pack cs con xs -> Pack cs con <$> traverseOf (traversed . atom) f xs

data LocalDef a = LocalDef {_localDefVar :: a, _localDefObj :: Obj a}
  deriving stock (Eq, Show, Functor, Foldable)

instance Pretty a => Pretty (LocalDef a) where
  pPrint (LocalDef v o) = pPrint v <+> "=" <+> pPrint o

localDefVar :: Lens' (LocalDef a) a
localDefVar = lens _localDefVar (\l v -> l {_localDefVar = v})

localDefObj :: Lens' (LocalDef a) (Obj a)
localDefObj = lens _localDefObj (\l o -> l {_localDefObj = o})

newtype Block a = Block {_statements :: [Stmt a]}
  deriving stock (Eq, Show, Functor, Foldable)

instance HasType a => HasType (Block a) where
  typeOf (Block stmts) = go stmts
    where
      go [] = VoidT
      go [Match r _ _] = typeOf r
      go (Return a : _) = typeOf a
      go (_ : rest) = go rest

-- typeOf (Match _ (c :| _)) = typeOf c
instance Pretty a => Pretty (Block a) where
  pPrint (Block stmts) = braces $ sep $ punctuate ";" $ map pPrint stmts

instance HasFreeVar Block where
  freevars (Block stmts) = HashSet.difference (foldMap freevars stmts) (foldMap bound stmts)

instance HasAtom Block where
  atom f (Block stmts) = Block <$> traverseOf (traversed . atom) f stmts

data Stmt a
  = -- | _1 = eval(expression)
    Eval a (Exp a)
  | -- | definition of local variables
    Let [LocalDef a]
  | -- | pattern matching
    Match
      a
      -- ^ result variable
      a
      -- ^ scrutinee
      (NonEmpty (Case a)) -- branches
  | -- | break from Match with the result value
    Break a
  | Return a
  deriving stock (Eq, Show, Functor, Foldable)

instance Pretty a => Pretty (Stmt a) where
  pPrint (Eval x e) = pPrint x <+> "=" <+> "eval" <+> pPrint e
  pPrint (Let xs) = "let" <+> brackets (sep (map pPrint xs))
  pPrint (Match r v cs) = pPrint r <+> "=" <+> parens ("match" <+> pPrint v $$ vcat (toList $ fmap pPrint cs))
  pPrint (Break x) = "break" <+> pPrint x
  pPrint (Return x) = "return" <+> pPrint x

instance HasFreeVar Stmt where
  freevars (Eval _ e) = freevars e
  freevars (Let xs) = foldr (sans . view localDefVar) (foldMap (freevars . view localDefObj) xs) xs
  freevars (Match _ v cs) = one v <> foldMap freevars cs
  freevars (Break x) = one x
  freevars (Return x) = one x

instance HasAtom Stmt where
  atom f = \case
    Eval x e -> Eval x <$> traverseOf atom f e
    Let xs -> Let <$> traverseOf (traversed . localDefObj . atom) f xs
    Match r v cs -> Match r v <$> traverseOf (traversed . atom) f cs
    s -> pure s

bound :: (Hashable a, Eq a) => Stmt a -> HashSet a
bound (Eval x _) = one x
bound (Let xs) = fromList $ map (view localDefVar) xs
bound (Match r _ _) = one r
bound (Break _) = mempty
bound (Return _) = mempty

appStmt :: Traversal' (Stmt a) (Exp a)
appStmt f = \case
  Eval x e -> Eval x <$> f e
  Let xs -> Let <$> traverseOf (traversed . localDefObj . appObj) f xs
  Match r v cs -> Match r v <$> traverseOf (traversed . appCase) f cs
  s -> pure s

appBlock :: Traversal' (Block a) (Exp a)
appBlock f Block {..} = Block <$> traverseOf (traversed . appStmt) f _statements

appObj :: Traversal' (Obj a) (Exp a)
appObj f = \case
  Fun ps e -> Fun ps <$> appBlock f e
  o -> pure o

appCase :: Traversal' (Case a) (Exp a)
appCase f = \case
  Unpack con ps e -> Unpack con ps <$> appBlock f e
  Switch u e -> Switch u <$> appBlock f e
  Bind x e -> Bind x <$> appBlock f e

appProgram :: Traversal' (Program a) (Exp a)
appProgram f Program {..} =
  Program _moduleName
    <$> traverseOf (traversed . _2 . appBlock) f _topVars
    <*> traverseOf (traversed . _2 . _2 . appBlock) f _topFuncs
    <*> pure _extFuncs
