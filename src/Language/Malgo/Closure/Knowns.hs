{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Malgo.Closure.Knowns (knownFuns) where

import Data.List (nub)
import Language.Malgo.Prelude
import Language.Malgo.TypedID
import Language.Malgo.FreeVars
import Language.Malgo.HIR
import Text.PrettyPrint hiding ((<>))

knownFuns :: Expr TypedID -> [TypedID]
knownFuns (Let (ExDec name _) body) =
  let fv = freevars body
  in if name `elem` fv
     then knownFuns body
     else name : knownFuns body
knownFuns (Let (FunDecs fd) body) =
  let fv = freevars body
      mayKnowns = knownFuns' fd
      ks = filter (not . flip elem fv) mayKnowns
  in ks <> knownFuns body
knownFuns (Let _ body) = knownFuns body
knownFuns (If _ t f) =
  knownFuns t <> knownFuns f
knownFuns _ = []

knownFuns' :: [FunDec TypedID] -> [TypedID]
knownFuns' fd =
  let fnNames = map (\(FunDec x _ _) -> x) fd
      fvs = map freevars fd
  in map fst $ filter (null . snd) $ zip fnNames fvs
