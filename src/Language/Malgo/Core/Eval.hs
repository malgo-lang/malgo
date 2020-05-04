{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Malgo.Core.Eval where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as V
import Language.Malgo.IR.Core
import Language.Malgo.Id
import Language.Malgo.Prelude
import Language.Malgo.Pretty
import Language.Malgo.TypeRep.CType
import Text.PrettyPrint.HughesPJ (parens, sep)

type Name = Id CType

type EvalM a = StateT Env IO a

data Env = Env {varMap :: Map Name Value}

data Value
  = FunV Int [Value] ([Value] -> EvalM Value)
  | PackV Con [Value]
  | ArrayV (IOVector Value)
  | UnboxedV Unboxed

instance Pretty Value where
  pPrint FunV {} = "<function>"
  pPrint (PackV con xs) = parens $ pPrint con <+> sep (map pPrint xs)
  pPrint (ArrayV _) = "<array>"
  pPrint (UnboxedV x) = pPrint x

evalProgram :: Program Name -> EvalM Value
evalProgram (Program mainId ds) = do
  traverse_ (uncurry loadDef) ds
  FunV _ _ mainFun <- lookupVar mainId
  mainFun []

runEval :: EvalM a -> IO a
runEval = evalStateT ?? Env mempty

defVar :: Name -> Value -> EvalM ()
defVar x v = modify (\e -> e {varMap = Map.insert x v (varMap e)})

loadDef :: Name -> Obj Name -> EvalM ()
loadDef x o = do
  o' <- evalObj o
  defVar x o'

lookupVar :: Name -> EvalM Value
lookupVar x = gets $ fromJust . Map.lookup x . varMap

evalObj :: Obj Name -> EvalM Value
evalObj (Fun ps e) =
  pure $ FunV (length ps) [] $ \ps' -> do
    env <- get
    zipWithM_ defVar ps ps'
    e' <- evalExp e
    put env
    pure e'
evalObj (Pack con xs) = do
  xs' <- traverse evalAtom xs
  pure $ PackV con xs'
evalObj (Array a n) = do
  a' <- evalAtom a
  n' <- evalAtom n
  case n' of
    UnboxedV (Int n'') -> do
      arr <- liftIO $ V.replicate (fromIntegral n'') a'
      pure $ ArrayV arr
    _ -> bug Unreachable

evalAtom :: Atom Name -> EvalM Value
evalAtom (Var x) = lookupVar x
evalAtom (Unboxed x) = pure $ UnboxedV x

evalExp :: Exp Name -> EvalM Value
evalExp (Atom x) = evalAtom x
evalExp (Call f xs) = do
  FunV n ys f' <- lookupVar f
  xs' <- traverse evalAtom xs
  if n >= length xs + length ys
    then f' (ys <> xs')
    else pure $ FunV n (ys <> xs') f'
evalExp (PrimCall prim _ xs) = do
  prim' <- lookupPrim prim
  xs' <- traverse evalAtom xs
  prim' xs'
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
  evalMatch e' cs

evalMatch :: Value -> NonEmpty (Case Name) -> EvalM Value
evalMatch (PackV con1 xs) (Unpack con2 ys e :| _)
  | con1 == con2 = do
    zipWithM_ defVar ys xs
    evalExp e
evalMatch v (Unpack {} :| (c : cs)) = evalMatch v (c :| cs)
evalMatch v (Bind x e :| _) = do
  defVar x v
  evalExp e
evalMatch _ _ = bug Unreachable

lookupPrim :: Text -> EvalM ([Value] -> EvalM Value)
lookupPrim "+" = pure $ \case
  [UnboxedV (Int x'), UnboxedV (Int y')] -> pure $ UnboxedV (Int $ x' + y')
  _ -> bug Unreachable
lookupPrim "-" = pure $ \case
  [UnboxedV (Int x'), UnboxedV (Int y')] -> pure $ UnboxedV (Int $ x' - y')
  _ -> bug Unreachable
lookupPrim "<=" = pure $ \case
  [x, y] ->
    let order = compareValue x y
     in pure $ boolToValue $ order == LT || order == EQ
  _ -> bug Unreachable
lookupPrim "print_int" = pure $ \case
  [PackV (Con "Tuple1" [PackT [Con "Int" [IntT]]]) [PackV (Con "Int" [IntT]) [UnboxedV (Int x)]]] -> do
    liftIO $ putStr $ show x
    pure unit
  _ -> bug Unreachable
lookupPrim "newline" = pure $ \case
  [PackV (Con "Tuple0" []) []] -> do
    liftIO $ putStrLn ""
    pure unit
  _ -> bug Unreachable
lookupPrim prim = error $ T.unpack $ "undefined primitive: " <> prim

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
boolToValue True = PackV (Con "True" []) []
boolToValue False = PackV (Con "False" []) []

unit :: Value
unit = PackV (Con "Tuple0" []) []
