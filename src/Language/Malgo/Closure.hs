{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Language.Malgo.Closure where

import           Control.Monad.State
import           Data.List
import           Language.Malgo.KNormal (Type)
import           Language.Malgo.MIR
import           Language.Malgo.Utils

data ClosureState = ClosureState { knowns   :: [Id]
                                 , toplevel :: [Instr]
                                 , count    :: Int
                                 }
  deriving Show

newtype Closure a = Closure (StateT ClosureState (Either String) a)
  deriving (Functor, Applicative, Monad, MonadState ClosureState)

runClosure (Closure m) i = runStateT m (ClosureState [] [] i)

fv :: Instr -> Closure [(Id, Type)]
fv (name, Fun [] params retTy body) = undefined
fv _                                = undefined

trans :: Block -> Closure Block
trans = undefined

-- 関数宣言をtoplevelに移動
-- 関数宣言の合った場所にMkClsを挿入
transInstr :: Instr -> Closure Instr
transInstr = undefined
