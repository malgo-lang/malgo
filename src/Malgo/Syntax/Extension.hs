{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Syntax.Extension
  ( MalgoPhase (..),
    Malgo,
    MalgoId,
    Visibility (..),
    Qualified (..),
    Field (..),
    Typed (..),
    PsId,
    RnId,
    Unboxed,
    Boxed,
    Assoc (..),
    XId,
    SimpleX,
    XVar,
    XCon,
    XUnboxed,
    XBoxed,
    XApply,
    XOpApp,
    XProject,
    XFn,
    XTuple,
    XRecord,
    XList,
    XRecordAccess,
    XAnn,
    XSeq,
    XParens,
    ForallExpX,
    XClause,
    ForallClauseX,
    XLet,
    XWith,
    XNoBind,
    ForallStmtX,
    XVarP,
    XConP,
    XTupleP,
    XRecordP,
    XListP,
    XUnboxedP,
    XBoxedP,
    ForallPatX,
    XTyApp,
    XTyVar,
    XTyCon,
    XTyArr,
    XTyTuple,
    XTyRecord,
    XTyBlock,
    ForallTypeX,
    XScDef,
    XScSig,
    XDataDef,
    XTypeSynonym,
    XInfix,
    XForeign,
    XImport,
    ImportList (..),
    ForallDeclX,
    XModule,
  )
where

import Control.Lens (lens)
import Data.Kind qualified as K
import Data.SCargot.Repr.Basic qualified as S
import Data.Store.TH
import Data.Void
import Malgo.Id
import Malgo.Infer.TypeRep as TypeRep
import Malgo.Module
import Malgo.Prelude hiding (All)
import Malgo.SExpr (ToSExpr (..))
import Malgo.SExpr qualified as S
import Prettyprinter ((<+>))

-- | Phase and type instance
data MalgoPhase = Parse | Rename | Infer | Refine | NewParse

data Malgo (p :: MalgoPhase)

-- | Id
type family MalgoId (p :: MalgoPhase) where
  MalgoId 'Parse = Text
  MalgoId 'Rename = Id
  MalgoId 'Infer = Id
  MalgoId 'Refine = Id
  MalgoId 'NewParse = Text

data Visibility
  = Explicit ModuleName -- variable that must be qualified
  | Implicit
  deriving stock (Show, Eq, Ord)

instance Pretty Visibility where pretty = pretty . convertString @_ @Text . show

-- | Qualified name
data Qualified x = Qualified {visibility :: Visibility, value :: x}
  deriving stock (Eq, Ord, Show)

instance (HasRange x) => HasRange (Qualified x) where
  range (Qualified _ v) = range v

instance (ToSExpr x) => ToSExpr (Qualified x) where
  toSExpr (Qualified Implicit v) = toSExpr v
  toSExpr (Qualified (Explicit x) v) = S.L [toSExpr x, toSExpr v]

instance (Pretty x) => Pretty (Qualified x) where
  pretty (Qualified Implicit v) = pretty v
  pretty (Qualified (Explicit x) v) = pretty x <> "." <> pretty v

-- | Type-annotated field
data Field x = Field {typeAnn :: Maybe Text, field :: x}
  deriving stock (Eq, Ord, Show)

instance (Pretty x) => Pretty (Field x) where
  pretty (Field Nothing v) = pretty v
  pretty (Field (Just x) v) = pretty x <> "." <> pretty v

data Typed x = Typed {annotated :: Type, value :: x}
  deriving stock (Eq, Ord, Show)

instance (Pretty x) => Pretty (Typed x) where
  pretty (Typed t v) = pretty v <+> ":" <+> pretty t

instance HasType (Typed x) where
  typeOf (Typed t _) = t
  types = lens (.annotated) (\x y -> x {annotated = y})

type PsId = XId (Malgo 'Parse)

type RnId = XId (Malgo 'Rename)

data Unboxed

data Boxed

data Assoc = LeftA | RightA | NeutralA
  deriving stock (Eq, Show, Generic)

instance ToSExpr Assoc where
  toSExpr LeftA = S.A $ S.Symbol "left"
  toSExpr RightA = S.A $ S.Symbol "right"
  toSExpr NeutralA = S.A $ S.Symbol "neutral"

instance Pretty Assoc where
  pretty LeftA = "l"
  pretty RightA = "r"
  pretty NeutralA = ""

type family XId x where
  XId (Malgo p) = MalgoId p

-- * Expr Extensions

type family SimpleX (x :: MalgoPhase) where
  SimpleX 'Parse = Range
  SimpleX 'Rename = SimpleX 'Parse
  SimpleX 'Infer = Typed (SimpleX 'Rename)
  SimpleX 'Refine = SimpleX 'Infer
  SimpleX 'NewParse = Range

type family XVar x where
  XVar (Malgo 'Parse) = Qualified (SimpleX 'Parse)
  XVar (Malgo x) = SimpleX x

type family XCon x where
  XCon (Malgo x) = SimpleX x

type family XUnboxed x where
  XUnboxed (Malgo x) = SimpleX x

type family XBoxed x where
  XBoxed (Malgo 'Parse) = SimpleX 'Parse
  XBoxed (Malgo 'NewParse) = SimpleX 'NewParse
  XBoxed (Malgo _) = Void

type family XApply x where
  XApply (Malgo x) = SimpleX x

type family XOpApp x where
  XOpApp (Malgo 'Parse) = SimpleX 'Parse
  XOpApp (Malgo NewParse) = SimpleX NewParse
  XOpApp (Malgo 'Rename) = (XOpApp (Malgo 'Parse), (Assoc, Int))
  XOpApp (Malgo 'Infer) = Typed (XOpApp (Malgo 'Rename))
  XOpApp (Malgo 'Refine) = Void

type family XProject x where
  XProject (Malgo Parse) = Void
  XProject (Malgo x) = SimpleX x

type family XFn x where
  XFn (Malgo x) = SimpleX x

type family XTuple x where
  XTuple (Malgo x) = SimpleX x

type family XRecord x where
  XRecord (Malgo x) = SimpleX x

type family XList x where
  XList (Malgo 'Parse) = SimpleX 'Parse
  XList (Malgo 'NewParse) = SimpleX 'NewParse
  XList (Malgo _) = Void

type family XRecordAccess x where
  XRecordAccess (Malgo x) = SimpleX x

type family XAnn x where
  XAnn (Malgo 'Parse) = SimpleX 'Parse
  XAnn (Malgo 'Rename) = SimpleX 'Rename
  XAnn (Malgo 'NewParse) = SimpleX 'NewParse
  XAnn (Malgo _) = Void

type family XSeq x where
  XSeq (Malgo x) = SimpleX x

type family XParens x where
  XParens (Malgo 'Parse) = SimpleX 'Parse
  XParens (Malgo NewParse) = SimpleX NewParse
  XParens (Malgo x) = Void

type ForallExpX (c :: K.Type -> Constraint) x =
  ( c (XVar x),
    c (XCon x),
    c (XUnboxed x),
    c (XBoxed x),
    c (XApply x),
    c (XOpApp x),
    c (XProject x),
    c (XFn x),
    c (XTuple x),
    c (XRecord x),
    c (XList x),
    c (XRecordAccess x),
    c (XAnn x),
    c (XSeq x),
    c (XParens x)
  )

-- * Clause Extensions

type family XClause x where
  XClause (Malgo x) = SimpleX x

type ForallClauseX (c :: K.Type -> Constraint) x = c (XClause x)

-- * Stmt Extensions

type family XLet x where
  XLet (Malgo _) = SimpleX 'Parse

type family XWith x where
  XWith (Malgo 'Parse) = SimpleX 'Parse
  XWith (Malgo NewParse) = SimpleX NewParse
  XWith (Malgo _) = Void

type family XNoBind x where
  XNoBind (Malgo _) = SimpleX 'Parse

type ForallStmtX (c :: K.Type -> Constraint) x = (c (XLet x), c (XWith x), c (XNoBind x))

-- * Pat Extensions

type family XVarP x where
  XVarP (Malgo x) = SimpleX x

type family XConP x where
  XConP (Malgo x) = SimpleX x

type family XTupleP x where
  XTupleP (Malgo x) = SimpleX x

type family XRecordP x where
  XRecordP (Malgo x) = SimpleX x

type family XListP x where
  XListP (Malgo 'Parse) = SimpleX 'Parse
  XListP (Malgo 'NewParse) = SimpleX 'NewParse
  XListP (Malgo _) = Void

type family XUnboxedP x where
  XUnboxedP (Malgo x) = SimpleX x

type family XBoxedP x where
  XBoxedP (Malgo 'Parse) = SimpleX 'Parse
  XBoxedP (Malgo NewParse) = SimpleX NewParse
  XBoxedP (Malgo _) = Void

type ForallPatX (c :: K.Type -> Constraint) x = (c (XVarP x), c (XConP x), c (XTupleP x), c (XRecordP x), c (XListP x), c (XUnboxedP x), c (XBoxedP x))

-- * Type Extensions

type family XTyApp x where
  XTyApp (Malgo _) = SimpleX 'Parse

type family XTyVar x where
  XTyVar (Malgo _) = SimpleX 'Parse

type family XTyCon x where
  XTyCon (Malgo NewParse) = Void
  XTyCon (Malgo _) = SimpleX 'Parse

type family XTyArr x where
  XTyArr (Malgo _) = SimpleX 'Parse

type family XTyTuple x where
  XTyTuple (Malgo _) = SimpleX 'Parse

type family XTyRecord x where
  XTyRecord (Malgo _) = SimpleX 'Parse

type family XTyBlock x where
  XTyBlock (Malgo 'Parse) = SimpleX 'Parse
  XTyBlock (Malgo 'NewParse) = SimpleX 'NewParse
  XTyBlock (Malgo _) = Void

type ForallTypeX (c :: K.Type -> Constraint) x =
  (c (XTyApp x), c (XTyVar x), c (XTyCon x), c (XTyArr x), c (XTyTuple x), c (XTyRecord x), c (XTyBlock x))

-- * Decl Extensions

type family XScDef x where
  XScDef (Malgo x) = SimpleX x

type family XScSig x where
  XScSig (Malgo _) = SimpleX 'Parse

type family XDataDef x where
  XDataDef (Malgo _) = SimpleX 'Parse

type family XTypeSynonym x where
  XTypeSynonym (Malgo _) = SimpleX 'Parse

type family XInfix x where
  XInfix (Malgo _) = SimpleX 'Parse

type family XForeign x where
  XForeign (Malgo 'Parse) = SimpleX 'Parse
  XForeign (Malgo NewParse) = SimpleX NewParse
  XForeign (Malgo 'Rename) = (XForeign (Malgo 'Parse), Text)
  XForeign (Malgo 'Infer) = Typed (XForeign (Malgo 'Rename))
  XForeign (Malgo 'Refine) = XForeign (Malgo 'Infer)

type family XImport x where
  XImport (Malgo _) = SimpleX 'Parse

data ImportList = All | Selected [PsId] | As ModuleName

deriving stock instance Eq ImportList

deriving stock instance Show ImportList

instance ToSExpr ImportList where
  toSExpr All = S.A $ S.Symbol "all"
  toSExpr (Selected ids) = S.L (S.A (S.Symbol "selected") : map toSExpr ids)
  toSExpr (As moduleName) = S.L [S.A (S.Symbol "as"), toSExpr moduleName]

type ForallDeclX (c :: K.Type -> Constraint) x =
  ( c (XScDef x),
    c (XScSig x),
    c (XDataDef x),
    c (XTypeSynonym x),
    c (XInfix x),
    c (XForeign x),
    c (XImport x),
    ForallExpX c x,
    ForallClauseX c x,
    ForallStmtX c x,
    ForallPatX c x,
    ForallTypeX c x
  )

-- * Module Extensions

type family XModule x

makeStore ''Assoc
