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
  scDefs <- foldMap (traverse toFunScDef) _scDefs
  dataDefs <- concat <$> traverse toFunDataDef _dataDefs
  foreigns <- traverse toFunForeign _foreigns
  pure $ Program $ scDefs <> dataDefs <> foreigns

toFunScDef :: (State Uniq :> es, Reader ModuleName :> es) => (Typed Range, Id, S.Expr (Malgo 'Refine)) -> Eff es (Range, Name, F.Expr)
toFunScDef (Typed {value = range}, name, expr) = do
  expr <- toFunExpr expr
  pure (range, name, expr)

toFunDataDef :: (State Uniq :> es, Reader ModuleName :> es) => (Range, Id, [(Range, Id)], [(Range, Id, [Type (Malgo 'Refine)])]) -> Eff es [(Range, Name, F.Expr)]
toFunDataDef (_, _, _, constructors) = traverse toFunConstructor constructors

toFunConstructor :: (State Uniq :> es, Reader ModuleName :> es) => (Range, Id, [Type (Malgo Refine)]) -> Eff es (Range, Name, F.Expr)
toFunConstructor (range, name, parameters) = do
  let arity = length parameters
  parameters <- replicateM arity $ newTemporalId "constructor"
  let lambda = foldr (\parameter -> F.Lambda range [parameter]) (F.Construct range (F.Tag $ name.name) (F.Var range <$> parameters)) parameters
  pure (range, name, lambda)

toFunForeign :: (State Uniq :> es, Reader ModuleName :> es) => (Typed (Range, Text), Id, Type (Malgo Refine)) -> Eff es (Range, Name, F.Expr)
toFunForeign (Typed {annotated = typ, value = (range, _)}, name, _) = do
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

toFunExpr :: (State Uniq :> es, Reader ModuleName :> es) => S.Expr (Malgo 'Refine) -> Eff es F.Expr
toFunExpr (S.Var Typed {value = range} name) | idIsExternal name = pure $ F.Invoke range name
toFunExpr (S.Var Typed {value = range} name) = pure $ F.Var range name
toFunExpr (S.Unboxed Typed {value = range} literal) = pure $ F.Literal range $ toFunLiteral literal
toFunExpr (S.Apply Typed {value = range} f x) = do
  f <- toFunExpr f
  x <- toFunExpr x
  pure $ F.Apply range f [x]
toFunExpr (S.Fn Typed {value = range} clauses@(head :| _)) = do
  parameters <- createParameters head
  body <- toFunClauses range parameters clauses
  pure $ go parameters body
  where
    createParameters (Clause _ patterns _) = replicateM (length patterns) $ newTemporalId "param"
    go [] body = body
    go (param : params) body = F.Lambda range [param] $ go params body
toFunExpr (S.Tuple Typed {value = range} exprs) = do
  exprs <- traverse toFunExpr exprs
  pure $ F.Construct range F.Tuple exprs
toFunExpr (S.Record Typed {value = range} fields) = do
  fields <- traverseOf (traverse . _2) toFunExpr fields
  pure $ F.Object range $ Map.fromList fields
toFunExpr (S.Seq _ stmts) = toFunStmts stmts

toFunStmts :: (State Uniq :> es, Reader ModuleName :> es) => NonEmpty (S.Stmt (Malgo 'Refine)) -> Eff es F.Expr
toFunStmts (NoBind _ expr :| []) = toFunExpr expr
toFunStmts (NoBind range value :| stmt : stmts) = do
  tmp <- newTemporalId "tmp"
  value <- toFunExpr value
  expr <- toFunStmts (stmt :| stmts)
  pure $ F.Apply range (F.Lambda range [tmp] expr) [value]
toFunStmts (S.Let range name value :| stmt : stmts) = do
  value <- toFunExpr value
  expr <- toFunStmts (stmt :| stmts)
  pure $ F.Let range name value expr
toFunStmts (S.Let _ _ value :| []) = toFunExpr value

toFunLiteral :: S.Literal Unboxed -> F.Literal
toFunLiteral (S.Int32 n) = F.Int32 n
toFunLiteral (S.Int64 n) = F.Int64 n
toFunLiteral (S.Float n) = F.Float n
toFunLiteral (S.Double n) = F.Double n
toFunLiteral (S.Char c) = F.Char c
toFunLiteral (S.String t) = F.String t

toFunClauses :: (State Uniq :> es, Reader ModuleName :> es) => Range -> [Name] -> NonEmpty (Clause (Malgo Refine)) -> Eff es F.Expr
toFunClauses range [parameter] clauses = do
  Select range (F.Var range parameter) <$> traverse toFunClause (toList clauses)
toFunClauses range parameters clauses = do
  Select range (Construct range F.Tuple (F.Var range <$> parameters)) <$> traverse toFunClause (toList clauses)

toFunClause :: (State Uniq :> es, Reader ModuleName :> es) => Clause (Malgo 'Refine) -> Eff es F.Branch
toFunClause (Clause Typed {value = range} [pattern] body) = do
  pattern <- toFunPattern pattern
  body <- toFunExpr body
  pure $ Branch range pattern body
toFunClause (Clause Typed {value = range} patterns body) = do
  patterns <- traverse toFunPattern patterns
  body <- toFunExpr body
  pure $ Branch range (Destruct range F.Tuple patterns) body

toFunPattern :: (State Uniq :> es, Reader ModuleName :> es) => S.Pat (Malgo 'Refine) -> Eff es F.Pattern
toFunPattern (VarP Typed {value = range} name) = pure $ PVar range name
toFunPattern (ConP Typed {value = range} tag patterns) = do
  patterns <- traverse toFunPattern patterns
  pure $ Destruct range (Tag tag.name) patterns
toFunPattern (TupleP Typed {value = range} patterns) = do
  patterns <- traverse toFunPattern patterns
  pure $ Destruct range F.Tuple patterns
toFunPattern (RecordP Typed {value = range} fields) = do
  fields <- traverseOf (traverse . _2) toFunPattern fields
  pure $ Expand range $ Map.fromList fields
toFunPattern (UnboxedP Typed {value = range} literal) = pure $ PLiteral range $ toFunLiteral literal