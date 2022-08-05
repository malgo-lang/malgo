{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Malgo.Syntax.Extension where

import Control.Lens (view)
import Data.Aeson
import Data.Binary (Binary)
import qualified Data.Kind as K
import Data.Void
import Koriel.Id
import Koriel.Pretty
import Language.LSP.Types.Lens (HasValue (value))
import Malgo.Prelude
import Malgo.TypeCheck.TypeRep as TypeRep

-- | Phase and type instance
data MalgoPhase = Parse | Rename | TypeCheck | Refine

data Malgo (p :: MalgoPhase)

-- | Id
type family MalgoId (p :: MalgoPhase) where
  MalgoId 'Parse = Text
  MalgoId 'Rename = Id ()
  MalgoId 'TypeCheck = Id ()
  MalgoId 'Refine = Id ()

-- | Qualified name
newtype WithPrefix x = WithPrefix {unwrapWithPrefix :: Annotated (Maybe Text) x}
  deriving stock (Eq, Ord, Show)

removePrefix :: WithPrefix a -> a
removePrefix = view value . unwrapWithPrefix

pattern NoPrefix :: x -> WithPrefix x
pattern NoPrefix x = WithPrefix (Annotated Nothing x)

pattern Prefix :: Text -> x -> WithPrefix x
pattern Prefix p x = WithPrefix (Annotated (Just p) x)

instance Pretty x => Pretty (WithPrefix x) where
  pPrint (WithPrefix (Annotated Nothing v)) = pPrint v
  pPrint (WithPrefix (Annotated (Just x) v)) = pPrint x <> "." <> pPrint v

type PsId = XId (Malgo 'Parse)

type RnId = XId (Malgo 'Rename)

data Unboxed

data Boxed

data Assoc = LeftA | RightA | NeutralA
  deriving stock (Eq, Show, Generic)

instance Binary Assoc

instance ToJSON Assoc

instance FromJSON Assoc

instance Pretty Assoc where
  pPrint LeftA = "l"
  pPrint RightA = "r"
  pPrint NeutralA = ""

type family XId x where
  XId (Malgo p) = MalgoId p

-- * Exp Extensions

type family SimpleX (x :: MalgoPhase) where
  SimpleX 'Parse = Range
  SimpleX 'Rename = SimpleX 'Parse
  SimpleX 'TypeCheck = Annotated Type (SimpleX 'Rename)
  SimpleX 'Refine = SimpleX 'TypeCheck

type family XVar x where
  XVar (Malgo 'Parse) = WithPrefix (SimpleX 'Parse)
  XVar (Malgo x) = SimpleX x

type family XCon x where
  XCon (Malgo x) = SimpleX x

type family XUnboxed x where
  XUnboxed (Malgo x) = SimpleX x

type family XBoxed x where
  XBoxed (Malgo 'Parse) = SimpleX 'Parse
  XBoxed (Malgo _) = Void

type family XApply x where
  XApply (Malgo x) = SimpleX x

type family XOpApp x where
  XOpApp (Malgo 'Parse) = SimpleX 'Parse
  XOpApp (Malgo 'Rename) = (XOpApp (Malgo 'Parse), (Assoc, Int))
  XOpApp (Malgo 'TypeCheck) = Annotated Type (XOpApp (Malgo 'Rename))
  XOpApp (Malgo 'Refine) = Void

type family XFn x where
  XFn (Malgo x) = SimpleX x

type family XTuple x where
  XTuple (Malgo x) = SimpleX x

type family XRecord x where
  XRecord (Malgo x) = SimpleX x

type family XList x where
  XList (Malgo 'Parse) = SimpleX 'Parse
  XList (Malgo _) = Void

type family XRecordAccess x where
  XRecordAccess (Malgo x) = SimpleX x

type family XAnn x where
  XAnn (Malgo 'Parse) = SimpleX 'Parse
  XAnn (Malgo 'Rename) = SimpleX 'Rename
  XAnn (Malgo _) = Void

type family XSeq x where
  XSeq (Malgo x) = SimpleX x

type family XParens x where
  XParens (Malgo x) = SimpleX x

type ForallExpX (c :: K.Type -> Constraint) x =
  ( c (XVar x),
    c (XCon x),
    c (XUnboxed x),
    c (XBoxed x),
    c (XApply x),
    c (XOpApp x),
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
  XListP (Malgo _) = Void

type family XUnboxedP x where
  XUnboxedP (Malgo x) = SimpleX x

type family XBoxedP x where
  XBoxedP (Malgo 'Parse) = SimpleX 'Parse
  XBoxedP (Malgo _) = Void

type ForallPatX (c :: K.Type -> Constraint) x = (c (XVarP x), c (XConP x), c (XTupleP x), c (XRecordP x), c (XListP x), c (XUnboxedP x), c (XBoxedP x))

-- * Type Extensions

type family XTyApp x where
  XTyApp (Malgo _) = SimpleX 'Parse

type family XTyVar x where
  XTyVar (Malgo _) = SimpleX 'Parse

type family XTyCon x where
  XTyCon (Malgo _) = SimpleX 'Parse

type family XTyArr x where
  XTyArr (Malgo _) = SimpleX 'Parse

type family XTyTuple x where
  XTyTuple (Malgo _) = SimpleX 'Parse

type family XTyRecord x where
  XTyRecord (Malgo _) = SimpleX 'Parse

type family XTyBlock x where
  XTyBlock (Malgo 'Parse) = SimpleX 'Parse
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
  XForeign (Malgo 'Rename) = (XForeign (Malgo 'Parse), Text)
  XForeign (Malgo 'TypeCheck) = Annotated Type (XForeign (Malgo 'Rename))
  XForeign (Malgo 'Refine) = XForeign (Malgo 'TypeCheck)

type family XImport x where
  XImport (Malgo _) = SimpleX 'Parse

data ImportList = All | Selected [PsId] | As ModuleName

deriving stock instance Eq ImportList

deriving stock instance Show ImportList

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
