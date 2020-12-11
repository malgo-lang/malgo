{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Griff.Syntax.Extension where

import Data.Kind (Constraint)
import qualified Data.Kind as K
import Data.Store
import Koriel.Id
import Koriel.Pretty
import Language.Griff.Prelude
import Language.Griff.Type
import Text.Megaparsec.Pos (SourcePos)

data Assoc = LeftA | RightA | NeutralA
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Store)

instance Pretty Assoc where
  pPrint LeftA = "l"
  pPrint RightA = "r"
  pPrint NeutralA = ""

type family XId x

-- Exp Extensions
type family XVar x

type family XCon x

type family XUnboxed x

type family XApply x

type family XOpApp x

type family XFn x

type family XTuple x

type family XForce x

type ForallExpX (c :: K.Type -> Constraint) x =
  ( c (XVar x),
    c (XCon x),
    c (XUnboxed x),
    c (XApply x),
    c (XOpApp x),
    c (XFn x),
    c (XTuple x),
    c (XForce x)
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

-- Phase and type instance
data GriffPhase = Parse | Rename | TypeCheck

data Griff (p :: GriffPhase)

newtype ModuleName = ModuleName String
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (Pretty)

instance Store ModuleName

_Module :: Lens' ModuleName String
_Module = lens (\(ModuleName s) -> s) (\_ s -> ModuleName s)

-- Id
type family GriffId (p :: GriffPhase) where
  GriffId 'Parse = String
  GriffId 'Rename = Id ModuleName
  GriffId 'TypeCheck = Id ModuleName

type family GriffTId (p :: GriffPhase) where
  GriffTId 'Parse = String
  GriffTId 'Rename = Id ModuleName
  GriffTId 'TypeCheck = Id ModuleName

type PsId = XId (Griff 'Parse)

type RnId = XId (Griff 'Rename)

type TcId = XId (Griff 'TypeCheck)

type PsTId = XTId (Griff 'Parse)

type RnTId = XTId (Griff 'Rename)

type TcTId = XTId (Griff 'TypeCheck)

type instance XVar (Griff 'Parse) = SourcePos

type instance XVar (Griff 'Rename) = SourcePos

type instance XVar (Griff 'TypeCheck) = WithType SourcePos

type instance XCon (Griff 'Parse) = SourcePos

type instance XCon (Griff 'Rename) = SourcePos

type instance XCon (Griff 'TypeCheck) = WithType SourcePos

type instance XId (Griff p) = GriffId p

type instance XUnboxed (Griff 'Parse) = SourcePos

type instance XUnboxed (Griff 'Rename) = SourcePos

type instance XUnboxed (Griff 'TypeCheck) = WithType SourcePos

type instance XApply (Griff 'Parse) = SourcePos

type instance XApply (Griff 'Rename) = SourcePos

type instance XApply (Griff 'TypeCheck) = WithType SourcePos

type instance XOpApp (Griff 'Parse) = SourcePos

type instance XOpApp (Griff 'Rename) = (SourcePos, (Assoc, Int))

type instance XOpApp (Griff 'TypeCheck) = WithType (SourcePos, (Assoc, Int))

type instance XFn (Griff 'Parse) = SourcePos

type instance XFn (Griff 'Rename) = SourcePos

type instance XFn (Griff 'TypeCheck) = WithType SourcePos

type instance XTuple (Griff 'Parse) = SourcePos

type instance XTuple (Griff 'Rename) = SourcePos

type instance XTuple (Griff 'TypeCheck) = WithType SourcePos

type instance XForce (Griff 'Parse) = SourcePos

type instance XForce (Griff 'Rename) = SourcePos

type instance XForce (Griff 'TypeCheck) = WithType SourcePos

type instance XClause (Griff 'Parse) = SourcePos

type instance XClause (Griff 'Rename) = SourcePos

type instance XClause (Griff 'TypeCheck) = WithType SourcePos

type instance XLet (Griff _) = SourcePos

type instance XNoBind (Griff _) = SourcePos

type instance XVarP (Griff 'Parse) = SourcePos

type instance XVarP (Griff 'Rename) = SourcePos

type instance XVarP (Griff 'TypeCheck) = WithType SourcePos

type instance XConP (Griff 'Parse) = SourcePos

type instance XConP (Griff 'Rename) = SourcePos

type instance XConP (Griff 'TypeCheck) = WithType SourcePos

type instance XTupleP (Griff 'Parse) = SourcePos

type instance XTupleP (Griff 'Rename) = SourcePos

type instance XTupleP (Griff 'TypeCheck) = WithType SourcePos

type instance XUnboxedP (Griff 'Parse) = SourcePos

type instance XUnboxedP (Griff 'Rename) = SourcePos

type instance XUnboxedP (Griff 'TypeCheck) = WithType SourcePos

type instance XTId (Griff p) = GriffTId p

type instance XTyApp (Griff _) = SourcePos

type instance XTyVar (Griff _) = SourcePos

type instance XTyCon (Griff _) = SourcePos

type instance XTyArr (Griff _) = SourcePos

type instance XTyTuple (Griff _) = SourcePos

type instance XTyLazy (Griff _) = SourcePos

type instance XScDef (Griff 'Parse) = SourcePos

type instance XScDef (Griff 'Rename) = SourcePos

type instance XScDef (Griff 'TypeCheck) = WithType SourcePos

type instance XScSig (Griff _) = SourcePos

type instance XDataDef (Griff _) = SourcePos

type instance XInfix (Griff _) = SourcePos

type instance XForeign (Griff 'Parse) = SourcePos

type instance XForeign (Griff 'Rename) = (SourcePos, String)

type instance XForeign (Griff 'TypeCheck) = WithType (SourcePos, String)

type instance XImport (Griff _) = SourcePos
