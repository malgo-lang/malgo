module Malgo.Sequent.ToFun (toFun) where

import Control.Lens (traverseOf, _2)
import Data.Map qualified as Map
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Malgo.Id
import Malgo.Infer.TypeRep qualified as R
import Malgo.Module
import Malgo.MonadUniq
import Malgo.Prelude
import Malgo.Sequent.Fun as F
import Malgo.Syntax as S
import Malgo.Syntax.Extension as S

toFun :: (State Uniq :> es, Reader ModuleName :> es) => XModule (Malgo 'Refine) -> Eff es Program
toFun BindGroup {..} = do
  scDefs <- foldMap (traverse fromScDef) _scDefs
  dataDefs <- concat <$> traverse fromDataDef _dataDefs
  foreigns <- traverse fromForeign _foreigns
  dependencies <- traverse getModuleName _imports
  pure
    $ Program
      { definitions = scDefs <> dataDefs <> foreigns,
        dependencies
      }
  where
    getModuleName (_, name, _) = pure name

fromScDef :: (State Uniq :> es, Reader ModuleName :> es) => (Typed Range, Id, S.Expr (Malgo 'Refine)) -> Eff es (Range, Name, F.Expr)
fromScDef (Typed {value = range}, name, expr) = do
  expr <- fromExpr expr
  pure (range, name, expr)

fromDataDef :: (State Uniq :> es, Reader ModuleName :> es) => (Range, Id, [(Range, Id)], [(Range, Id, [Type (Malgo 'Refine)])]) -> Eff es [(Range, Name, F.Expr)]
fromDataDef (_, _, _, constructors) = traverse fromConstructor constructors

fromConstructor :: (State Uniq :> es, Reader ModuleName :> es) => (Range, Id, [Type (Malgo Refine)]) -> Eff es (Range, Name, F.Expr)
fromConstructor (range, name, parameters) = do
  let arity = length parameters
  parameters <- replicateM arity $ newTemporalId "constructor"
  let lambda = foldr (\parameter -> F.Lambda range [parameter]) (F.Construct range (F.Tag name.name) (F.Var range <$> parameters)) parameters
  pure (range, name, lambda)

fromForeign :: (State Uniq :> es, Reader ModuleName :> es) => (Typed (Range, Text), Id, Type (Malgo Refine)) -> Eff es (Range, Name, F.Expr)
fromForeign (Typed {annotated = typ, value = (range, _)}, name, _) = do
  case typ of
    R.TyArr {} -> do
      let arity = aux typ
      parameters <- replicateM arity $ newTemporalId "primitive"
      let primitive = foldr (\parameter -> F.Lambda range [parameter]) (F.Primitive range name.name $ map (F.Var range) parameters) parameters
      pure (range, name, primitive)
    _ -> error "invalid type"
  where
    aux (R.TyArr _ t) = 1 + aux t
    aux _ = 0

fromExpr :: (State Uniq :> es, Reader ModuleName :> es) => S.Expr (Malgo 'Refine) -> Eff es F.Expr
fromExpr (S.Var Typed {value = range} name) | idIsExternal name = pure $ F.Invoke range name
fromExpr (S.Var Typed {value = range} name) = pure $ F.Var range name
fromExpr (S.Unboxed Typed {value = range} literal) = pure $ F.Literal range $ fromLiteral literal
fromExpr (S.Apply Typed {value = range} f x) = do
  f <- fromExpr f
  x <- fromExpr x
  pure $ F.Apply range f [x]
fromExpr (S.Fn Typed {value = range} clauses@(head :| _)) = do
  parameters <- createParameters head
  body <- fromClauses range parameters clauses
  pure $ go parameters body
  where
    createParameters (Clause _ patterns _) = replicateM (length patterns) $ newTemporalId "param"
    go [] body = body
    go (param : params) body = F.Lambda range [param] $ go params body
fromExpr (S.Tuple Typed {value = range} exprs) = do
  exprs <- traverse fromExpr exprs
  pure $ F.Construct range F.Tuple exprs
fromExpr (S.Record Typed {value = range} fields) = do
  fields <- traverseOf (traverse . _2) fromExpr fields
  pure $ F.Object range $ Map.fromList fields
fromExpr (S.Seq _ stmts) = fromStmts stmts

fromStmts :: (State Uniq :> es, Reader ModuleName :> es) => NonEmpty (S.Stmt (Malgo 'Refine)) -> Eff es F.Expr
fromStmts (NoBind _ expr :| []) = fromExpr expr
fromStmts (NoBind range value :| stmt : stmts) = do
  tmp <- newTemporalId "tmp"
  value <- fromExpr value
  expr <- fromStmts (stmt :| stmts)
  pure $ F.Apply range (F.Lambda range [tmp] expr) [value]
fromStmts (S.Let range name value :| stmt : stmts) = do
  value <- fromExpr value
  expr <- fromStmts (stmt :| stmts)
  pure $ F.Let range name value expr
fromStmts (S.Let _ _ value :| []) = fromExpr value

fromLiteral :: S.Literal Unboxed -> F.Literal
fromLiteral (S.Int32 n) = F.Int32 n
fromLiteral (S.Int64 n) = F.Int64 n
fromLiteral (S.Float n) = F.Float n
fromLiteral (S.Double n) = F.Double n
fromLiteral (S.Char c) = F.Char c
fromLiteral (S.String t) = F.String t

fromClauses :: (State Uniq :> es, Reader ModuleName :> es) => Range -> [Name] -> NonEmpty (Clause (Malgo Refine)) -> Eff es F.Expr
fromClauses range [parameter] clauses = do
  Select range (F.Var range parameter) <$> traverse fromClause (toList clauses)
fromClauses range parameters clauses = do
  Select range (Construct range F.Tuple (F.Var range <$> parameters)) <$> traverse fromClause (toList clauses)

fromClause :: (State Uniq :> es, Reader ModuleName :> es) => Clause (Malgo 'Refine) -> Eff es F.Branch
fromClause (Clause Typed {value = range} [pattern] body) = do
  pattern <- fromPattern pattern
  body <- fromExpr body
  pure $ Branch range pattern body
fromClause (Clause Typed {value = range} patterns body) = do
  patterns <- traverse fromPattern patterns
  body <- fromExpr body
  pure $ Branch range (Destruct range F.Tuple patterns) body

fromPattern :: (State Uniq :> es, Reader ModuleName :> es) => S.Pat (Malgo 'Refine) -> Eff es F.Pattern
fromPattern (VarP Typed {value = range} name) = pure $ PVar range name
fromPattern (ConP Typed {value = range} tag patterns) = do
  patterns <- traverse fromPattern patterns
  pure $ Destruct range (Tag tag.name) patterns
fromPattern (TupleP Typed {value = range} patterns) = do
  patterns <- traverse fromPattern patterns
  pure $ Destruct range F.Tuple patterns
fromPattern (RecordP Typed {value = range} fields) = do
  fields <- traverseOf (traverse . _2) fromPattern fields
  pure $ Expand range $ Map.fromList fields
fromPattern (UnboxedP Typed {value = range} literal) = pure $ PLiteral range $ fromLiteral literal