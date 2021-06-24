{-# LANGUAGE UndecidableInstances #-}

module Malgo.Syntax.Extension where

import Data.Binary (Binary)
import Data.Kind (Constraint)
import qualified Data.Kind as K
import Data.Void
import Koriel.Id
import Koriel.Pretty
import Malgo.Prelude
import qualified Malgo.TypeRep.Static as S
import qualified Malgo.TypeRep.UTerm as U
import Text.Megaparsec.Pos (SourcePos)

-- Phase and type instance
data MalgoPhase = Parse | Rename | TypeCheck | Refine

data Malgo (p :: MalgoPhase)

-- Id
type family MalgoId (p :: MalgoPhase) where
  MalgoId 'Parse = String
  MalgoId 'Rename = Id ()
  MalgoId 'TypeCheck = Id ()
  MalgoId 'Refine = Id ()

type PsId = XId (Malgo 'Parse)

type RnId = XId (Malgo 'Rename)

data Unboxed

data Boxed

data Assoc = LeftA | RightA | NeutralA
  deriving stock (Eq, Show, Generic)

instance Binary Assoc

instance Pretty Assoc where
  pPrint LeftA = "l"
  pPrint RightA = "r"
  pPrint NeutralA = ""

type family XId x where
  XId (Malgo p) = MalgoId p

-- Exp Extensions

type family SimpleX (x :: MalgoPhase) where
  SimpleX 'Parse = SourcePos
  SimpleX 'Rename = SourcePos
  SimpleX 'TypeCheck = With U.UType SourcePos
  SimpleX 'Refine = With S.Type SourcePos

type family XVar x where
  XVar (Malgo x) = SimpleX x

type family XCon x where
  XCon (Malgo x) = SimpleX x

type family XUnboxed x where
  XUnboxed (Malgo x) = SimpleX x

type family XBoxed x where
  XBoxed (Malgo 'Parse) = SourcePos
  XBoxed (Malgo _) = Void

type family XApply x where
  XApply (Malgo x) = SimpleX x

type family XOpApp x where
  XOpApp (Malgo 'Parse) = SourcePos
  XOpApp (Malgo 'Rename) = (SourcePos, (Assoc, Int))
  XOpApp (Malgo 'TypeCheck) = With U.UType (SourcePos, (Assoc, Int))
  XOpApp (Malgo 'Refine) = Void

type family XFn x where
  XFn (Malgo x) = SimpleX x

type family XTuple x where
  XTuple (Malgo x) = SimpleX x

type family XRecord x where
  XRecord (Malgo x) = SimpleX x

type family XForce x where
  XForce (Malgo x) = SimpleX x

type family XRecordAccess x where
  XRecordAccess (Malgo x) = SimpleX x

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
    c (XForce x),
    c (XRecordAccess x),
    c (XParens x)
  )

-- Clause Extensions
type family XClause x where
  XClause (Malgo x) = SimpleX x

type ForallClauseX (c :: K.Type -> Constraint) x = c (XClause x)

-- Stmt Extensions

type family XLet x where
  XLet (Malgo _) = SourcePos

type family XNoBind x where
  XNoBind (Malgo _) = SourcePos

type ForallStmtX (c :: K.Type -> Constraint) x = (c (XLet x), c (XNoBind x))

-- Pat Extensions
type family XVarP x where
  XVarP (Malgo x) = SimpleX x

type family XConP x where
  XConP (Malgo x) = SimpleX x

type family XTupleP x where
  XTupleP (Malgo x) = SimpleX x

type family XRecordP x where
  XRecordP (Malgo x) = SimpleX x

type family XUnboxedP x where
  XUnboxedP (Malgo x) = SimpleX x

type ForallPatX (c :: K.Type -> Constraint) x = (c (XVarP x), c (XConP x), c (XTupleP x), c (XRecordP x), c (XUnboxedP x))

-- Type Extensions
type family XTyApp x where
  XTyApp (Malgo _) = SourcePos

type family XTyVar x where
  XTyVar (Malgo _) = SourcePos

type family XTyCon x where
  XTyCon (Malgo _) = SourcePos

type family XTyArr x where
  XTyArr (Malgo _) = SourcePos

type family XTyTuple x where
  XTyTuple (Malgo _) = SourcePos

type family XTyRecord x where
  XTyRecord (Malgo _) = SourcePos

type family XTyLazy x where
  XTyLazy (Malgo _) = SourcePos

type ForallTypeX (c :: K.Type -> Constraint) x =
  (c (XTyApp x), c (XTyVar x), c (XTyCon x), c (XTyArr x), c (XTyTuple x), c (XTyRecord x), c (XTyLazy x))

-- Decl Extensions
type family XScDef x where
  XScDef (Malgo x) = SimpleX x

type family XScSig x where
  XScSig (Malgo _) = SourcePos

type family XDataDef x where
  XDataDef (Malgo _) = SourcePos

type family XTypeSynonym x where
  XTypeSynonym (Malgo _) = SourcePos

type family XInfix x where
  XInfix (Malgo _) = SourcePos

type family XForeign x where
  XForeign (Malgo 'Parse) = SourcePos
  XForeign (Malgo 'Rename) = (SourcePos, String)
  XForeign (Malgo 'TypeCheck) = With U.UType (SourcePos, String)
  XForeign (Malgo 'Refine) = With S.Type (SourcePos, String)

type family XImport x where
  XImport (Malgo _) = SourcePos

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

-- Module Extensions
type family XModule x
