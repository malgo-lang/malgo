{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.Closure.Knowns (knownFuns) where

import           Language.Malgo.FreeVars
import           Language.Malgo.ID
import           Language.Malgo.IR.HIR
import           Language.Malgo.Prelude

knownFuns :: Expr TypedID -> [TypedID]
knownFuns (Let (ExDec name _) body) =
  if name `elem` freevars body
  then knownFuns body
  else name : knownFuns body
knownFuns (Let (FunDecs fd) body) =
  filter (not . flip elem (freevars body)) (knownFuns' fd)
  <> knownFuns body
knownFuns (Let _ body) = knownFuns body
knownFuns (If _ t f) =
  knownFuns t <> knownFuns f
knownFuns _ = []

knownFuns' :: [FunDec TypedID] -> [TypedID]
knownFuns' fd =
  map fst $ filter (null . snd) $ zip fnNames fvs
  where fnNames = map (\(FunDec x _ _) -> x) fd
        fvs = map freevars fd
