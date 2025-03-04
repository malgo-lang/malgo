module Malgo.Sequent.ToFun (toFun) where

import Control.Lens (traverseOf, _2)
import Data.Map qualified as Map
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Local (State)
import Malgo.Id
import Malgo.Module
import Malgo.MonadUniq
import Malgo.Prelude
import Malgo.Sequent.Fun as F
import Malgo.Syntax as S
import Malgo.Syntax.Extension as S

toFun :: (State Uniq :> es, Reader ModuleName :> es) => XModule (Malgo 'Refine) -> Eff es Program
toFun BindGroup {..} = do
  scDefs <- foldMap (traverse convert) _scDefs
  dataDefs <- concat <$> traverse convert _dataDefs
  pure $ Program $ scDefs <> dataDefs

class Convert a b where
  convert :: a -> b

instance (State Uniq :> es, Reader ModuleName :> es) => Convert (Typed Range, Id, S.Expr (Malgo 'Refine)) (Eff es (Range, Name, F.Expr)) where
  convert (Typed {value = range}, name, expr) = do
    expr <- convert expr
    pure (range, name, expr)

instance (State Uniq :> es, Reader ModuleName :> es) => Convert (Range, Id, [(Range, Id)], [(Range, Id, [Type (Malgo 'Refine)])]) (Eff es [(Range, Name, F.Expr)]) where
  convert (_, _, _, constructors) = do
    traverse convert constructors

instance (State Uniq :> es, Reader ModuleName :> es) => Convert (Range, Id, [Type (Malgo Refine)]) (Eff es (Range, Name, F.Expr)) where
  convert (range, name, parameters) = do
    let arity = length parameters
    parameters <- replicateM arity $ newTemporalId "constructor"
    let lambda = foldr (\parameter -> F.Lambda range [parameter]) (F.Construct range (F.Tag $ name.name) (F.Var range <$> parameters)) parameters
    pure (range, name, lambda)

instance (State Uniq :> es, Reader ModuleName :> es) => Convert (S.Expr (Malgo 'Refine)) (Eff es F.Expr) where
  convert (S.Var Typed {value = range} name) | idIsExternal name = pure $ F.Invoke range name
  convert (S.Var Typed {value = range} name) = pure $ F.Var range name
  convert (S.Unboxed Typed {value = range} literal) = pure $ F.Literal range $ convert literal
  convert (S.Apply Typed {value = range} f x) = do
    f <- convert f
    x <- convert x
    pure $ F.Apply range f [x]
  convert (S.Fn Typed {value = range} clauses@(head :| _)) = do
    parameters <- createParameters head
    body <- convert range parameters clauses
    pure $ go parameters body
    where
      createParameters (Clause _ patterns _) = replicateM (length patterns) $ newTemporalId "param"
      go [] body = body
      go (param : params) body = F.Lambda range [param] $ go params body
  convert (S.Tuple Typed {value = range} exprs) = do
    exprs <- traverse convert exprs
    pure $ F.Construct range F.Tuple exprs
  convert (S.Record Typed {value = range} fields) = do
    fields <- traverseOf (traverse . _2) convert fields
    pure $ F.Object range $ Map.fromList fields
  convert (S.Seq _ stmts) = convert stmts

instance (State Uniq :> es, Reader ModuleName :> es) => Convert (NonEmpty (S.Stmt (Malgo 'Refine))) (Eff es F.Expr) where
  convert (NoBind _ expr :| []) = convert expr
  convert (NoBind range value :| stmt : stmts) = do
    tmp <- newTemporalId "tmp"
    value <- convert value
    expr <- convert (stmt :| stmts)
    pure $ F.Apply range (F.Lambda range [tmp] expr) [value]
  convert (S.Let range name value :| stmt : stmts) = do
    value <- convert value
    expr <- convert (stmt :| stmts)
    pure $ F.Let range name value expr
  convert (S.Let _ _ value :| []) = convert value

instance Convert (S.Literal Unboxed) F.Literal where
  convert (S.Int32 n) = F.Int32 n
  convert (S.Int64 n) = F.Int64 n
  convert (S.Float n) = F.Float n
  convert (S.Double n) = F.Double n
  convert (S.Char c) = F.Char c
  convert (S.String t) = F.String t

instance (State Uniq :> es, Reader ModuleName :> es) => Convert Range ([Name] -> NonEmpty (Clause (Malgo Refine)) -> Eff es F.Expr) where
  convert range [parameter] clauses = do
    Select range (F.Var range parameter) <$> traverse convert (toList clauses)
  convert range parameters clauses = do
    Select range (Construct range F.Tuple (F.Var range <$> parameters)) <$> traverse convert (toList clauses)

instance (State Uniq :> es, Reader ModuleName :> es) => Convert (Clause (Malgo 'Refine)) (Eff es F.Branch) where
  convert (Clause Typed {value = range} [pattern] body) = do
    pattern <- convert pattern
    body <- convert body
    pure $ Branch range pattern body
  convert (Clause Typed {value = range} patterns body) = do
    patterns <- traverse convert patterns
    body <- convert body
    pure $ Branch range (Destruct range F.Tuple patterns) body

instance (State Uniq :> es, Reader ModuleName :> es) => Convert (S.Pat (Malgo 'Refine)) (Eff es F.Pattern) where
  convert (VarP Typed {value = range} name) = pure $ PVar range name
  convert (ConP Typed {value = range} tag patterns) = do
    patterns <- traverse convert patterns
    pure $ Destruct range (Tag tag.name) patterns
  convert (TupleP Typed {value = range} patterns) = do
    patterns <- traverse convert patterns
    pure $ Destruct range F.Tuple patterns
  convert (RecordP Typed {value = range} fields) = do
    fields <- traverseOf (traverse . _2) convert fields
    pure $ Expand range $ Map.fromList fields
  convert (UnboxedP Typed {value = range} literal) = pure $ PLiteral range $ convert literal