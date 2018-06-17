{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.Closure.Knowns (knownFuns) where

import Language.Malgo.Prelude
import Language.Malgo.TypedID
import Language.Malgo.FreeVars
import Language.Malgo.IR.HIR

knownFuns :: Expr TypedID -> [TypedID]
knownFuns (Let (ExDec name _) body) =
  if name `elem` fv
  then knownFuns body
  else name : knownFuns body
  where fv = freevars body
knownFuns (Let (FunDecs fd) body) =
  filter (not . flip elem fv) mayKnowns
  <> knownFuns body
  where fv = freevars body
        mayKnowns = knownFuns' fd
knownFuns (Let _ body) = knownFuns body
knownFuns (If _ t f) =
  knownFuns t <> knownFuns f
knownFuns _ = []

knownFuns' :: [FunDec TypedID] -> [TypedID]
knownFuns' fd =
  map fst $ filter (null . snd) $ zip fnNames fvs
  where fnNames = map (\(FunDec x _ _) -> x) fd
        fvs = map freevars fd
