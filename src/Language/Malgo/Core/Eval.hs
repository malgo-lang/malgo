{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Core.Eval where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as V
import Language.Malgo.IR.Core
import Language.Malgo.IR.Op
import Language.Malgo.Id
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.CType
import Text.PrettyPrint.HughesPJ (parens, sep)

type Name = Id CType

type EvalM a = StateT Env IO a

data Env
  = Env
      { varMap :: Map Name Value,
        funMap :: Map Name ([Value] -> EvalM Value)
      }

data Value
  = FunV ([Value] -> EvalM Value)
  | PackV Int [Value]
  | ArrayV (IOVector Value)
  | UnboxedV Unboxed

instance Pretty Value where
  pPrint FunV {} = "<function>"
  pPrint (PackV con xs) = parens $ pPrint con <+> sep (map pPrint xs)
  pPrint (ArrayV _) = "<array>"
  pPrint (UnboxedV x) = pPrint x

evalProgram :: Program Name -> EvalM Value
evalProgram Program {topBinds, mainExp, topFuncs} = do
  traverse_ (uncurry loadFun) topFuncs
  traverse_ (uncurry loadDef) topBinds
  evalExp mainExp

runEval :: EvalM a -> IO a
runEval = evalStateT ?? Env mempty mempty

defVar :: Name -> Value -> EvalM ()
defVar x v = modify (\e -> e {varMap = Map.insert x v (varMap e)})

loadFun :: MonadState Env m => Name -> ([Name], Exp Name) -> m ()
loadFun x (ps, e) = do
  let fun ps' = do
        env <- get
        zipWithM_ defVar ps ps'
        e' <- evalExp e
        put env
        pure e'
  modify (\env -> env {funMap = Map.insert x fun (funMap env)})

loadDef :: Name -> Obj Name -> EvalM ()
loadDef x (Fun ps e) = do
  Env {varMap = capture} <- get
  defVar x $ FunV $ \ps' -> do
    env <- get
    modify $ const $ env {varMap = capture <> varMap env}
    zipWithM_ defVar ps ps'
    e' <- evalExp e
    put env
    pure e'
loadDef x (Pack con xs) =
  case cTypeOf x of
    PackT union -> do
      let tag = Set.findIndex con union
      xs' <- traverse evalAtom xs
      defVar x $ PackV tag xs'
    _ -> bug Unreachable
loadDef x (Array a n) = do
  a' <- evalAtom a
  n' <- evalAtom n
  case n' of
    UnboxedV (Int n'') -> do
      arr <- liftIO $ V.replicate (fromIntegral n'') a'
      defVar x $ ArrayV arr
    _ -> bug Unreachable

lookupVar :: HasCallStack => Name -> EvalM Value
lookupVar x = do
  x' <- gets $ Map.lookup x . varMap
  case x' of
    Just x'' -> pure x''
    Nothing -> error $ show $ pPrint x <> " is not defined"

lookupFun :: HasCallStack => MonadState Env m => Name -> m ([Value] -> EvalM Value)
lookupFun x = do
  x' <- gets $ Map.lookup x . funMap
  case x' of
    Just x'' -> pure x''
    Nothing -> error $ show $ pPrint x <> " is not defined"

evalAtom :: Atom Name -> EvalM Value
evalAtom (Var x) = lookupVar x
evalAtom (Unboxed x) = pure $ UnboxedV x

evalExp :: Exp Name -> EvalM Value
evalExp (Atom x) = evalAtom x
evalExp (Call (Var f) xs) = do
  FunV f' <- lookupVar f
  xs' <- traverse evalAtom xs
  f' xs'
evalExp (Call v _) =
  error $ show (pPrint v) <> " is not callable"
evalExp (CallDirect f xs) = do
  f' <- lookupFun f
  xs' <- traverse evalAtom xs
  f' xs'
evalExp (PrimCall prim _ xs) = do
  prim' <- lookupPrim prim
  xs' <- traverse evalAtom xs
  prim' xs'
evalExp (BinOp o x y) = evalOp o <$> evalAtom x <*> evalAtom y
evalExp (ArrayRead a i) = do
  ArrayV vec <- evalAtom a
  UnboxedV (Int idx) <- evalAtom i
  liftIO $ V.read vec $ fromIntegral idx
evalExp (ArrayWrite a i v) = do
  ArrayV vec <- evalAtom a
  UnboxedV (Int idx) <- evalAtom i
  v' <- evalAtom v
  liftIO $ V.write vec (fromIntegral idx) v'
  pure unit
evalExp (Let xs e) = do
  traverse_ (uncurry loadDef) xs
  evalExp e
evalExp (Match e cs) = do
  e' <- evalExp e
  evalMatch (cTypeOf e) e' cs

evalOp :: Op -> Value -> Value -> Value
evalOp Add (UnboxedV (Int x)) (UnboxedV (Int y)) = UnboxedV $ Int $ x + y
evalOp Sub (UnboxedV (Int x)) (UnboxedV (Int y)) = UnboxedV $ Int $ x - y
evalOp Mul (UnboxedV (Int x)) (UnboxedV (Int y)) = UnboxedV $ Int $ x * y
evalOp Div (UnboxedV (Int x)) (UnboxedV (Int y)) = UnboxedV $ Int $ x `div` y
evalOp Mod (UnboxedV (Int x)) (UnboxedV (Int y)) = UnboxedV $ Int $ x `mod` y
evalOp FAdd (UnboxedV (Float x)) (UnboxedV (Float y)) = UnboxedV $ Float $ x + y
evalOp FSub (UnboxedV (Float x)) (UnboxedV (Float y)) = UnboxedV $ Float $ x - y
evalOp FMul (UnboxedV (Float x)) (UnboxedV (Float y)) = UnboxedV $ Float $ x * y
evalOp FDiv (UnboxedV (Float x)) (UnboxedV (Float y)) = UnboxedV $ Float $ x / y
evalOp Eq x y = boolToValue $ compareValue x y == EQ
evalOp Neq x y = boolToValue $ compareValue x y /= EQ
evalOp Lt x y = boolToValue $ compareValue x y == LT
evalOp Le x y = boolToValue $ compareValue x y == LT || compareValue x y == EQ
evalOp Gt x y = boolToValue $ compareValue x y == GT
evalOp Ge x y = boolToValue $ compareValue x y == GT || compareValue x y == EQ
evalOp _ _ _ = bug Unreachable

evalMatch :: CType -> Value -> NonEmpty (Case Name) -> EvalM Value
evalMatch _ v (Bind x e :| _) = do
  defVar x v
  evalExp e
evalMatch (PackT union) (PackV tag xs) (Unpack con ys e :| rest) = do
  let tag' = Set.findIndex con union
  if tag == tag'
    then do
      zipWithM_ defVar ys xs
      evalExp e
    else case rest of
      (c : cs) -> evalMatch (PackT union) (PackV tag xs) (c :| cs)
      _ -> bug Unreachable
evalMatch _ _ _ = bug Unreachable

lookupPrim :: Text -> EvalM ([Value] -> EvalM Value)
lookupPrim "+" = pure $ \case
  [UnboxedV (Int x'), UnboxedV (Int y')] -> pure $ UnboxedV (Int $ x' + y')
  _ -> bug Unreachable
lookupPrim "-" = pure $ \case
  [UnboxedV (Int x'), UnboxedV (Int y')] -> pure $ UnboxedV (Int $ x' - y')
  _ -> bug Unreachable
lookupPrim "<" = pure $ \case
  [x, y] -> pure $ boolToValue $ compareValue x y == LT
  _ -> bug Unreachable
lookupPrim "<=" = pure $ \case
  [x, y] ->
    let order = compareValue x y
     in pure $ boolToValue $ order == LT || order == EQ
  _ -> bug Unreachable
lookupPrim "==" = pure $ \case
  [x, y] ->
    let order = compareValue x y
     in pure $ boolToValue $ order == EQ
  _ -> bug Unreachable
lookupPrim "print_int" = pure $ \case
  [PackV 0 [UnboxedV (Int x)]] -> do
    liftIO $ putStr $ show x
    pure unit
  _ -> bug Unreachable
lookupPrim "print_bool" = pure $ \case
  [x] -> ifThenElse x (liftIO $ putStr "true") (liftIO $ putStr "false") >> pure unit
  _ -> bug Unreachable
lookupPrim "newline" = pure $ \case
  [] -> do
    liftIO $ putStrLn ""
    pure unit
  _ -> bug Unreachable
lookupPrim prim = error $ T.unpack $ "undefined primitive: " <> prim

ifThenElse :: Value -> p -> p -> p
ifThenElse c t f =
  case (c, boolToValue True) of
    (PackV b _, PackV true _)
      | b == true -> t
      | otherwise -> f
    _ -> bug Unreachable

compareValue :: Value -> Value -> Ordering
compareValue (UnboxedV x) (UnboxedV y) = compare x y
compareValue (PackV con1 xs) (PackV con2 ys)
  | con1 == con2 = go xs ys
  where
    go [] [] = EQ
    go (a : as) (b : bs) = case compareValue a b of
      EQ -> go as bs
      order -> order
    go _ _ = bug Unreachable
compareValue _ _ = bug Unreachable

boolToValue :: Bool -> Value
boolToValue True = PackV (Set.findIndex (Con "True" []) $ Set.fromList [Con "True" [], Con "False" []]) []
boolToValue False = PackV (Set.findIndex (Con "False" []) $ Set.fromList [Con "True" [], Con "False" []]) []

unit :: Value
unit = PackV 0 []
