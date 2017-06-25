{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Syntax (Name, Type, Exp(..), sample) where

import           Control.Monad.State (evalState)
import           Symbol

type Name = Symbol
data Type = UnitT
          | IntT
          | BoolT
          | FloatT
          | StringT
  deriving Show

type TypedVar = (Name, Type)

{-
MinTigerとでも呼ぶべきか
e ::=
  c 定数
  (op e_1 ... e_n) プリミティブ演算
  (if e_1 e_2 e_3) 条件分岐
  (let (dec) e) 変数,関数定義
  (e_1 ... e_n) 関数呼び出し

dec :: =
  (function name (x_1:t_1 ... x_n:t_n) : t e) 関数宣言
  (var name : t e) 変数宣言
-}
data Exp = Unit
         | Int Int
         | Bool Bool
         | String String
         | Float Double
         | Not Exp
         | Neg Exp
         | Add Exp Exp
         | Sub Exp Exp
         | FNeg Exp Exp
         | FAdd Exp Exp
         | FSub Exp Exp
         | FMul Exp Exp
         | FDiv Exp Exp
         | Eq Exp Exp
         | Lt Exp Exp
         | Gt Exp Exp
         | Le Exp Exp
         | Ge Exp Exp
         | Call Name [Exp]
         | Var Name
         | If Exp Exp Exp
         | Let [Dec] [Exp]
  deriving Show

data Dec = FunDec TypedVar [TypedVar] [Exp]
         | VarDec TypedVar Exp
  deriving Show

{-
(let ((function add (x:int y:int) : int
        (+ x y)))
  (+ 3 4))
-}

sample = (`evalState` empty) $ do
  add <- symbol "add"
  x <- symbol "x"
  y <- symbol "y"
  return (Let [FunDec (add, IntT) [(x, IntT), (y, IntT)]
                [Add (Var x) (Var y)]]
           [Call add [Int 3, Int 4]])
