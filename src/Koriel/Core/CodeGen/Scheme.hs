{-# LANGUAGE TemplateHaskell #-}

module Koriel.Core.CodeGen.Scheme where

import Control.Lens (makeFieldsNoPrefix)
import Data.String.Conversions
import Koriel.Core.Op (Op (..))
import Koriel.Core.Syntax as Core
import Koriel.Core.Type
import Koriel.Id
import Koriel.MonadUniq
import Koriel.Prelude
import Koriel.Scheme.AST as Scheme
import Numeric.Extra (floatToDouble)

type MonadCodeGen m = (MonadIO m, MonadReader CGEnv m, MonadState CGState m)

data CGEnv = CGEnv
  { _moduleName :: ModuleName,
    _uniqSupply :: UniqSupply
  }

data CGState = CGState

makeFieldsNoPrefix ''CGEnv
makeFieldsNoPrefix ''CGState

codeGen :: MonadIO m => UniqSupply -> Program (Id Type) -> m [Expr]
codeGen _uniqSupply Program {..} = runReaderT (evalStateT aux initialState) initialEnv
  where
    initialEnv = CGEnv {..}
    initialState = CGState
    aux = do
      -- exts <- traverse genExtFunc _extFuncs
      funcs <- traverse genFunc _topFuncs
      vars <- traverse genVar _topVars
      pure $ funcs <> vars

-- genExtFunc :: MonadCodeGen m => (Text, Type) -> m Expr
-- genExtFunc (_, _) = undefined

genFunc :: MonadCodeGen m => (Id Type, ([Id Type], Exp (Id Type))) -> m Expr
genFunc (name, (params, body)) = do
  let name' = fromId name
  let params' = map fromId params
  body' <- genExp body
  pure $ Define name' (Just params') body'

genVar :: MonadCodeGen m => (Id Type, Exp (Id Type)) -> m Expr
genVar (name, body) = do
  let name' = fromId name
  body' <- genExp body
  pure $ Define name' Nothing body'

genExp :: MonadCodeGen m => Exp (Id Type) -> m Expr
genExp (Atom atom) = genAtom atom
genExp (Core.Call func args) = do
  func' <- genAtom func
  args' <- traverse genAtom args
  pure $ Scheme.Call func' args'
genExp (CallDirect func args) = do
  let func' = Variable (fromId func)
  args' <- traverse genAtom args
  pure $ Scheme.Call func' args'
genExp (RawCall func _ args) = do
  let func' = Variable $ Identifier $ convertString func
  args' <- traverse genAtom args
  pure $ Scheme.Call func' args'
genExp (BinOp op lhs rhs) = do
  lhs' <- genAtom lhs
  rhs' <- genAtom rhs
  op' <- genOp op
  pure $ op' lhs' rhs'
genExp (Cast _ e) = genAtom e
genExp (Let binds body) = do
  binds' <- traverse genBind binds
  body' <- genExp body
  pure $ Scheme.LetRec binds' body'
genExp (Core.Match e alts) = do
  e <- genExp e
  alts <- genAlts e alts
  pure $ Cond alts
genExp (Core.Error typ) = pure $ Scheme.Error $ "internal error: " <> show typ

-- | Compile Core match alternatives to Scheme cond.
--  It generates `cond` expression from these alternatives:
--  1. The clause that checks the tag of the scrutinee and binds the corresponding constructor arguments.
--  2. checks the scrutinee is a record and binds the corresponding fields.
--  3. checks the scrutinee is the unboxed value.
--  4. Simply binds the scrutinee to the variable.
genAlts :: MonadCodeGen m => Expr -> NonEmpty (Case (Id Type)) -> m [Clause]
genAlts scrutinee alts = genAlts' (toList alts)
  where
    genAlts' [] = pure []

genBind :: MonadCodeGen m => LocalDef (Id Type) -> m Binding
genBind = undefined

genOp :: MonadCodeGen m => Op -> m (Expr -> Expr -> Expr)
genOp Add = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "+") [lhs, rhs]
genOp Sub = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "-") [lhs, rhs]
genOp Mul = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "*") [lhs, rhs]
genOp Div =
  -- use `floor-quotient` instead of `/` to avoid problems with real numbers
  pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "floor-quotient") [lhs, rhs]
genOp Mod =
  -- use `floor-remainder` instead of `modulo` or `mod` to avoid problems with real numbers
  pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "floor-remainder") [lhs, rhs]
genOp FAdd = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "+") [lhs, rhs]
genOp FSub = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "-") [lhs, rhs]
genOp FMul = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "*") [lhs, rhs]
genOp FDiv = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "/") [lhs, rhs]
genOp Eq = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "=") [lhs, rhs]
genOp Neq = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "not") [Scheme.Call (Variable $ Identifier "=") [lhs, rhs]]
genOp Lt = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "<") [lhs, rhs]
genOp Le = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "<=") [lhs, rhs]
genOp Gt = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier ">") [lhs, rhs]
genOp Ge = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier ">=") [lhs, rhs]
genOp And = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "and") [lhs, rhs]
genOp Or = pure $ \lhs rhs -> Scheme.Call (Variable $ Identifier "or") [lhs, rhs]

genAtom :: MonadCodeGen m => Atom (Id Type) -> m Expr
genAtom (Var v) = pure $ Variable (fromId v)
genAtom (Unboxed u) = pure $
  Literal $ case u of
    Int32 i -> Integer i
    Int64 i -> Integer i
    Core.Float f -> Scheme.Float $ floatToDouble f
    Core.Double f -> Scheme.Float f
    Core.Char c -> Scheme.Char c
    Core.String s -> Scheme.String $ convertString s
    Bool b -> Boolean b
