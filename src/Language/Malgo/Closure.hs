{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Language.Malgo.Closure where

import           Control.Monad.State
import           Data.List
import           Language.Malgo.KNormal (Type (..))
import           Language.Malgo.MIR     hiding (count)
import           Language.Malgo.Utils

data ClosureState = ClosureState { knowns   :: [Id]
                                 , toplevel :: [Instr]
                                 , closure  :: [(Id, (Id, Type))]
                                 , count    :: Int
                                 }
  deriving Show

newtype Closure a = Closure (StateT ClosureState (Either String) a)
  deriving (Functor, Applicative, Monad, MonadState ClosureState)

runClosure :: Int -> Closure Block -> Either String (Block, ClosureState)
runClosure i (Closure m) = runStateT m (ClosureState [] [] [] i)

resetKnowns :: Closure ()
resetKnowns = modify $ \e -> e { knowns = [] }

addKnowns :: Id -> Closure ()
addKnowns name = modify $ \e -> e { knowns = name : knowns e }

isKnown :: Id -> Closure Bool
isKnown name =
  elem name <$> gets knowns

freeVars :: Instr -> Closure [(Id, Type)]
freeVars ((name, _), Fun [] params retTy body) = do
  addKnowns name

  backup <- get
  resetKnowns

  addKnowns name
  mapM_ (addKnowns . fst) params
  kns <- gets knowns

  tmp <- concat <$> mapM freeVars (blockBody body)

  put backup

  return $ filter (\(x, _) -> x `notElem` kns) tmp

-- freeVars ((name, _), Var name' typ) = do
--   addKnowns name
--   is_known <- isKnown name'
--   if is_known
--     then return []
--     else return [(name', typ)]
freeVars ((name, _), App fn params) = do
  addKnowns name
  filterM (\(x, _) -> isnotKnown x) (fn : params)
  where isnotKnown x = not <$> isKnown x
freeVars ((name, _), AppCls fn params) = do
  addKnowns name
  filterM (\(x, _) -> isnotKnown x) (fn : params)
  where isnotKnown x = not <$> isKnown x
freeVars ((name, _), MkCls fn params) = do
  addKnowns name
  filterM (\(x, _) -> isnotKnown x) (fn : params)
  where isnotKnown x = not <$> isKnown x
freeVars ((name, _), If c t f) = do
  addKnowns name
  is_known_c <- isKnown (fst c)
  t_fv <- concat <$> mapM freeVars (blockBody t)
  f_fv <- concat <$> mapM freeVars (blockBody f)
  if is_known_c
    then return $ union t_fv f_fv
    else return $ c : union t_fv f_fv
freeVars ((name, _), BinOp _ x y) = do
  addKnowns name
  filterM (isnotKnown . fst) [x, y]
  where isnotKnown a = not <$> isKnown a
freeVars ((name, _), _) = addKnowns name >> return []

trans :: Block -> Closure Block
trans (Block label body) = do
  Block label <$> transInstrs body

newCls :: (Id, Type) -> [(Id, Type)] -> Closure Instr
newCls fn fv = do
  c <- gets count
  modify $ \e -> e { count = count e + 1 }
  let new = Id (c, fromId (fst fn))
  insertClosure (fst fn) (new, ClsTy (snd fn))
  return ((new, ClsTy (snd fn)), MkCls fn fv)

insertToplevel :: Instr -> Closure ()
insertToplevel instr = modify $ \e -> e { toplevel = instr : toplevel e }
insertKnown :: Id -> Closure ()
insertKnown name = modify $ \e -> e { knowns = name : knowns e }
insertClosure :: Id -> (Id, Type) -> Closure ()
insertClosure name cls = modify $ \e -> e { closure = (name, cls) : closure e }

-- 関数宣言をtoplevelに移動
-- 関数宣言の合った場所にMkClsを挿入
transInstrs :: [Instr] -> Closure [Instr]
transInstrs (def@((name, ty), Fun _ params retTy body) : xs) = do
  -- bodyに自由変数が含まれていれば、MkClsに置き換え
  -- 普通に呼び出せる関数だったら、そのまま削除
  fv <- freeVars def
  fun <- Fun fv params retTy <$> (resetKnowns >> trans body)
  insertToplevel ((name, ty), fun)
  if null fv
    then insertKnown name >> transInstrs xs
    else (:) <$> newCls (name, ty) fv <*> transInstrs xs

transInstrs (((name, ty), App fn params) : xs) = do
  cls <- gets closure
  case lookup (fst fn) cls of
    Just c  -> (:) ((name, ty), AppCls c params) <$> transInstrs xs
    Nothing -> (:) ((name, ty), App fn params) <$> transInstrs xs

-- ついでに文字列定数をトップレベルに持ち上げる
transInstrs (constStr@((_, _), String _) : xs) = do
  insertToplevel constStr
  transInstrs xs

transInstrs (x:xs) = (:) <$> return x <*> transInstrs xs
transInstrs [] =
  return []
