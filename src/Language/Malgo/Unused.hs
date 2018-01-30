{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.Unused where

import           Debug.Trace
import           Language.Malgo.MIR
import           Language.Malgo.Type

used :: Eq a => Expr a -> [a]
used (Var x)                     = [x]
used (Tuple xs)                  = xs
used (TupleAccess x _)           = [x]
used (CallDir _ xs)              = xs
used (CallCls x xs)              = x:xs
used (Let (ValDec _ e) body)     = used e ++ used body
used (Let (ClsDec _ _ cls) body) = cls ++ used body
used (If c t f)                  = [c] ++ used t ++ used f
used (BinOp _ x y)               = [x, y]
used _                           = []

remove' :: (Typeable a, Eq a) => Expr a -> Expr a
remove' (Let dec@(ValDec a _) body) =
  if a `notElem` used body && (typeOf a /= "Unit")
  then remove' body
  else Let dec (remove' body)
remove' (Let dec@(ClsDec a _ _) body) =
  if a `notElem` used body
  then remove' body
  else Let dec (remove' body)
remove' (If c t f) = If c (remove' t) (remove' f)
remove' x = x

remove :: (Typeable a, Eq a) => Program a -> Program a
remove (Program fundec exdec body) =
  Program (map (\(FunDec n a f body) ->
                  FunDec n a f (remove' body)) fundec)
  exdec
  (remove' body)
