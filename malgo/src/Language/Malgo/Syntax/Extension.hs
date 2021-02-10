{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Syntax.Extension where

import Data.Binary (Binary)
import Data.Kind (Constraint)
import qualified Data.Kind as K
import Koriel.Id
import Koriel.Pretty
import Language.Malgo.Prelude
import Language.Malgo.Type
import Text.Megaparsec.Pos (SourcePos)

data Unboxed

data Boxed

data Assoc = LeftA | RightA | NeutralA
  deriving stock (Eq, Show, Generic)

instance Binary Assoc

instance Pretty Assoc where
  pPrint LeftA = "l"
  pPrint RightA = "r"
  pPrint NeutralA = ""

type family XId x

-- Exp Extensions
type family XVar x

type family XCon x

type family XUnboxed x

type family XBoxed x

type family XApply x

type family XOpApp x

type family XFn x

type family XTuple x

type family XForce x

type family XParens x

type ForallExpX (c :: K.Type -> Constraint) x =
  ( c (XVar x),
    c (XCon x),
    c (XUnboxed x),
    c (XBoxed x),
    c (XApply x),
    c (XOpApp x),
    c (XFn x),
    c (XTuple x),
    c (XForce x),
    c (XParens x)
  )

-- Clause Extensions
type family XClause x

type ForallClauseX (c :: K.Type -> Constraint) x = c (XClause x)

-- Stmt Extensions

type family XLet x

type family XNoBind x

type ForallStmtX (c :: K.Type -> Constraint) x = (c (XLet x), c (XNoBind x))

-- Pat Extensions
type family XVarP x

type family XConP x

type family XTupleP x

type family XUnboxedP x

type ForallPatX (c :: K.Type -> Constraint) x = (c (XVarP x), c (XConP x), c (XTupleP x), c (XUnboxedP x))

-- Type Extensions
type family XTId x

type family XTyApp x

type family XTyVar x

type family XTyCon x

type family XTyArr x

type family XTyTuple x

type family XTyLazy x

type ForallTypeX (c :: K.Type -> Constraint) x =
  (c (XTyApp x), c (XTyVar x), c (XTyCon x), c (XTyArr x), c (XTyTuple x), c (XTyLazy x))

-- Decl Extensions
type family XScDef x

type family XScSig x

type family XDataDef x

type family XInfix x

type family XForeign x

type family XImport x

type ForallDeclX (c :: K.Type -> Constraint) x =
  ( c (XScDef x),
    c (XScSig x),
    c (XDataDef x),
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

-- Phase and type instance
data MalgoPhase = Parse | Rename | TypeCheck

data Malgo (p :: MalgoPhase)

newtype ModuleName = ModuleName String
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (Pretty)

instance Binary ModuleName

_Module :: Lens' ModuleName String
_Module = lens (\(ModuleName s) -> s) (\_ s -> ModuleName s)

-- Id
type family MalgoId (p :: MalgoPhase) where
  MalgoId 'Parse = String
  MalgoId 'Rename = Id ModuleName
  MalgoId 'TypeCheck = Id ModuleName

type family MalgoTId (p :: MalgoPhase) where
  MalgoTId 'Parse = String
  MalgoTId 'Rename = Id ModuleName
  MalgoTId 'TypeCheck = Id ModuleName

type PsId = XId (Malgo 'Parse)

type RnId = XId (Malgo 'Rename)

type TcId = XId (Malgo 'TypeCheck)

type PsTId = XTId (Malgo 'Parse)

type RnTId = XTId (Malgo 'Rename)

type TcTId = XTId (Malgo 'TypeCheck)

type instance XVar (Malgo 'Parse) = SourcePos

type instance XVar (Malgo 'Rename) = SourcePos

type instance XVar (Malgo 'TypeCheck) = WithType SourcePos

type instance XCon (Malgo 'Parse) = SourcePos

type instance XCon (Malgo 'Rename) = SourcePos

type instance XCon (Malgo 'TypeCheck) = WithType SourcePos

type instance XId (Malgo p) = MalgoId p

type instance XUnboxed (Malgo 'Parse) = SourcePos

type instance XUnboxed (Malgo 'Rename) = SourcePos

type instance XUnboxed (Malgo 'TypeCheck) = WithType SourcePos

type instance XBoxed (Malgo 'Parse) = SourcePos

type instance XBoxed (Malgo 'Rename) = SourcePos

type instance XBoxed (Malgo 'TypeCheck) = WithType SourcePos

type instance XApply (Malgo 'Parse) = SourcePos

type instance XApply (Malgo 'Rename) = SourcePos

type instance XApply (Malgo 'TypeCheck) = WithType SourcePos

type instance XOpApp (Malgo 'Parse) = SourcePos

type instance XOpApp (Malgo 'Rename) = (SourcePos, (Assoc, Int))

type instance XOpApp (Malgo 'TypeCheck) = WithType (SourcePos, (Assoc, Int))

type instance XFn (Malgo 'Parse) = SourcePos

type instance XFn (Malgo 'Rename) = SourcePos

type instance XFn (Malgo 'TypeCheck) = WithType SourcePos

type instance XTuple (Malgo 'Parse) = SourcePos

type instance XTuple (Malgo 'Rename) = SourcePos

type instance XTuple (Malgo 'TypeCheck) = WithType SourcePos

type instance XForce (Malgo 'Parse) = SourcePos

type instance XForce (Malgo 'Rename) = SourcePos

type instance XForce (Malgo 'TypeCheck) = WithType SourcePos

type instance XParens (Malgo 'Parse) = SourcePos

type instance XParens (Malgo 'Rename) = SourcePos

type instance XParens (Malgo 'TypeCheck) = WithType SourcePos

type instance XClause (Malgo 'Parse) = SourcePos

type instance XClause (Malgo 'Rename) = SourcePos

type instance XClause (Malgo 'TypeCheck) = WithType SourcePos

type instance XLet (Malgo _) = SourcePos

type instance XNoBind (Malgo _) = SourcePos

type instance XVarP (Malgo 'Parse) = SourcePos

type instance XVarP (Malgo 'Rename) = SourcePos

type instance XVarP (Malgo 'TypeCheck) = WithType SourcePos

type instance XConP (Malgo 'Parse) = SourcePos

type instance XConP (Malgo 'Rename) = SourcePos

type instance XConP (Malgo 'TypeCheck) = WithType SourcePos

type instance XTupleP (Malgo 'Parse) = SourcePos

type instance XTupleP (Malgo 'Rename) = SourcePos

type instance XTupleP (Malgo 'TypeCheck) = WithType SourcePos

type instance XUnboxedP (Malgo 'Parse) = SourcePos

type instance XUnboxedP (Malgo 'Rename) = SourcePos

type instance XUnboxedP (Malgo 'TypeCheck) = WithType SourcePos

type instance XTId (Malgo p) = MalgoTId p

type instance XTyApp (Malgo _) = SourcePos

type instance XTyVar (Malgo _) = SourcePos

type instance XTyCon (Malgo _) = SourcePos

type instance XTyArr (Malgo _) = SourcePos

type instance XTyTuple (Malgo _) = SourcePos

type instance XTyLazy (Malgo _) = SourcePos

type instance XScDef (Malgo 'Parse) = SourcePos

type instance XScDef (Malgo 'Rename) = SourcePos

type instance XScDef (Malgo 'TypeCheck) = WithType SourcePos

type instance XScSig (Malgo _) = SourcePos

type instance XDataDef (Malgo _) = SourcePos

type instance XInfix (Malgo _) = SourcePos

type instance XForeign (Malgo 'Parse) = SourcePos

type instance XForeign (Malgo 'Rename) = (SourcePos, String)

type instance XForeign (Malgo 'TypeCheck) = WithType (SourcePos, String)

type instance XImport (Malgo _) = SourcePos
