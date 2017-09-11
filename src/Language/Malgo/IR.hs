module Language.Malgo.IR where

import           Control.Applicative
import           Control.Arrow         ((&&&))
import           Control.Monad
import           Data.Either
import qualified Language.Malgo.Syntax as S

data IR = Var S.Name
        | Int Integer
        | Float Double
        | Bool Bool
        | Char Char
        | String String
        | Defvar S.Name Type IR
        | Defun S.Name Func
        | Call S.Name [IR]
        | Block [IR]
        deriving (Eq, Show)

data Func = Func Type [(S.Name, Type)] [IR]
  deriving (Eq, Show)

data Type = TSym S.Name
          | TList [Type]
          deriving (Eq, Show)

-- TODO: rewrite 'translate' as 'translate :: S.AST -> Either String IR'
translate :: S.AST -> Either String IR
translate (S.Symbol x) = return $ Var x
translate (S.Int x)    = return $ Int x
translate (S.Float x)  = return $ Float x
translate (S.Bool x)   = return $ Bool x
translate (S.Char x)   = return $ Char x
translate (S.String x) = return $ String x
translate (S.List xs)  = translist xs
translate x            = Left $ "\nnot a valid form: " ++ S.pretty x


translist :: [S.AST] -> Either String IR
translist [S.Symbol "define", S.Typed (S.Symbol var) typ, val] = Defvar var <$> transtype typ <*> translate val

translist (S.Symbol "define" : S.List (x:xs) : body)
  | isTyped x && all isTyped xs = Defun funcName <$> (Func <$> retType <*> params <*> body')
  where isTyped :: S.AST -> Bool
        isTyped (S.Typed (S.Symbol _) _) = True
        isTyped _                        = False
        funcName = S._name . S._val $ x
        retType = transtype . S._type $ x
        params = let names = map (S._name . S._val) xs
                     types = map (transtype . S._type) xs
                 in if null (lefts types)
                    then Right $ zip names (rights types)
                    else Left (concat (lefts types))
        body' = let blk = map translate body
                in if null (lefts blk)
                   then Right (rights blk)
                   else Left (concat (lefts blk))


translist xs@(S.Symbol "define" : _) = Left ("\nnot a valid declaration: " ++ S.pretty (S.List xs))
translist (S.Symbol f : args) = Call f <$> args'
  where args' = let a = map translate args
                in if null (lefts a)
                   then Right $ rights a
                   else Left $ concat (lefts a)
translist x = Left ("\nnot a valid form: " ++ S.pretty (S.List x))


transtype :: S.AST -> Either String Type
transtype (S.Symbol x) = Right $ TSym x
transtype (S.List xs)  = do
  let xs' = map transtype xs
  if null (lefts xs')
    then Right $ TList (rights xs')
    else Left $ concat (lefts xs')
transtype x            = Left ("\nnot a type: " ++ S.pretty x)
