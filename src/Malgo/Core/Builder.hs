{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Malgo.Core.Builder where

import Control.Lens (ifor)
import Data.Traversable (for)
import Effectful
import Effectful.Writer.Static.Local (Writer, runWriter, tell)
import GHC.Stack (HasCallStack)
import Malgo.Core
import Malgo.Location
import Malgo.Name
import Malgo.Prelude
import Malgo.Unique

var :: (HasCallStack) => Name -> Eff es Producer
var = pure . Var fromCallStack

literal :: (HasCallStack) => Literal -> Eff es Producer
literal = pure . Literal fromCallStack

construct :: (HasCallStack) => Text -> [Eff es Producer] -> [Eff es Consumer] -> Eff es Producer
construct name args conts = do
  args' <- sequence args
  conts' <- sequence conts
  pure $ Construct fromCallStack name args' conts'

comatch :: (HasCallStack, UniqueGen :> es) => [Eff es (Copattern, Producer)] -> Eff es Producer
comatch branches = do
  branches' <- ifor branches \i clause -> do
    ret <- newName $ "comatch_ret_" <> show i
    ((tag, params, rets), body) <- clause
    pure ((tag, params, rets <> [ret]), Cut fromCallStack body (Label fromCallStack ret))
  pure $ Comatch fromCallStack branches'

finish :: (HasCallStack) => Eff es Consumer
finish = pure $ Finish fromCallStack

label :: (HasCallStack) => Name -> Eff es Consumer
label = pure . Label fromCallStack

destruct :: (HasCallStack, UniqueGen :> es) => Eff es Producer -> Text -> [Eff es Producer] -> [Eff es Consumer] -> Eff es Producer
destruct target name args conts = do
  target' <- target
  args' <- sequence args
  conts' <- sequence conts
  ret <- newName "destruct_ret"
  pure
    $ Do fromCallStack ret
    $ Cut fromCallStack target'
    $ Destruct fromCallStack name args' (conts' <> [Label fromCallStack ret])

match :: (HasCallStack, UniqueGen :> es) => Eff es Producer -> [Eff es (Pattern, Producer)] -> Eff es Producer
match scrutinee branches = do
  scrutinee' <- scrutinee
  ret <- newName "match_ret"
  branches' <- for branches \clause -> do
    ((tag, params, rets), body) <- clause
    pure ((tag, params, rets), Cut fromCallStack body (Label fromCallStack ret))
  pure
    $ Do fromCallStack ret
    $ Cut fromCallStack scrutinee'
    $ Match fromCallStack branches'

branch :: (UniqueGen :> es) => Text -> [Text] -> [Text] -> ([Name] -> [Name] -> Eff es Producer) -> Eff es (Pattern, Producer)
branch name params rets body = do
  params' <- traverse newName params
  rets' <- traverse newName rets
  body' <- body params' rets'
  pure ((name, params', rets'), body')

prim :: (HasCallStack, UniqueGen :> es) => Text -> [Eff es Producer] -> Eff es Producer
prim name args = do
  args' <- sequence args
  ret <- newName "prim_ret"
  pure $ Do fromCallStack ret (Prim fromCallStack name args' (Label fromCallStack ret))

switch :: (HasCallStack, UniqueGen :> es) => Eff es Producer -> [(Literal, Eff es Producer)] -> Eff es Producer -> Eff es Producer
switch scrutinee branches defaultBranch = do
  scrutinee' <- scrutinee
  branches' <- sequence [(lit,) <$> body | (lit, body) <- branches]
  defaultBranch' <- defaultBranch
  ret <- newName "switch_ret"
  let branches'' = [(lit, Cut fromCallStack body (Label fromCallStack ret)) | (lit, body) <- branches']
  let defaultBranch'' = Cut fromCallStack defaultBranch' (Label fromCallStack ret)
  pure
    $ Do fromCallStack ret
    $ Switch fromCallStack scrutinee' branches'' defaultBranch''

bind :: (HasCallStack, UniqueGen :> es) => Text -> Eff es Producer -> (Name -> Eff es Producer) -> Eff es Producer
bind name producer body = do
  name' <- newName name
  producer' <- producer
  body' <- body name'
  ret <- newName "bind_ret"
  pure
    $ Do fromCallStack ret
    $ Cut fromCallStack producer'
    $ Then fromCallStack name'
    $ Cut fromCallStack body' (Label fromCallStack ret)

def :: (HasCallStack, UniqueGen :> es, Writer [Definition] :> es) => Text -> [Text] -> [Text] -> (Name -> [Name] -> [Name] -> Eff es Producer) -> Eff es Name
def name params returns body = do
  name' <- newName name
  params' <- traverse newName params
  returns' <- traverse newName returns
  ret <- newName "def_ret"
  body' <- body name' params' returns'
  tell [Definition name' params' (returns' <> [ret]) (Cut fromCallStack body' (Label fromCallStack ret))]
  pure name'

invoke :: (HasCallStack, UniqueGen :> es) => Name -> [Eff es Producer] -> [Eff es Consumer] -> Eff es Producer
invoke name args conts = do
  args' <- sequence args
  conts' <- sequence conts
  ret <- newName "invoke_ret"
  pure $ Do fromCallStack ret (Invoke fromCallStack name args' (conts' <> [Label fromCallStack ret]))

goto :: (HasCallStack, UniqueGen :> es) => Eff es Producer -> Eff es Consumer -> Eff es Producer
goto term cont = do
  term' <- term
  cont' <- cont
  name <- newName "goto"
  pure $ Do fromCallStack name (Cut fromCallStack term' cont')

labelOf :: (HasCallStack, UniqueGen :> es) => Text -> (Name -> Eff es Producer) -> Eff es Producer
labelOf name body = do
  name' <- newName name
  body' <- body name'
  pure $ Do fromCallStack name' (Cut fromCallStack body' (Label fromCallStack name'))

toplevel :: (HasCallStack) => Eff es Producer -> Eff es Statement
toplevel body = do
  body' <- body
  pure $ Cut fromCallStack body' (Finish fromCallStack)

ex1 :: (UniqueGen :> es) => Eff es (Statement, [Definition])
ex1 = runWriter @[Definition] do
  _ <- def "fac" ["n"] [] \fac [n] [] -> do
    switch
      (var n)
      [(Int 0, literal $ Int 1)]
      $ prim "mul" [var n, invoke fac [prim "sub" [var n, literal $ Int 1]] []]
  _ <- def "prod" ["n", "m"] [] \prod [n, m] [] -> do
    switch
      (var n)
      [(Int 0, literal $ Int 0)]
      $ switch
        (prim "sub" [var n, literal $ Int 1])
        [(Int 0, var m)]
      $ prim "add" [var m, invoke prod [prim "sub" [var n, literal $ Int 1], var m] []]
  monus <- def "monus" ["n", "m"] [] \monus [n, m] [] -> do
    switch
      (var m)
      [(Int 0, var n)]
      $ switch
        (var n)
        [(Int 0, literal $ Int 0)]
      $ invoke monus [prim "sub" [var n, literal $ Int 1], prim "sub" [var m, literal $ Int 1]] []
  toplevel (invoke monus [literal $ Int 10, literal $ Int 5] [])

ex2 :: (UniqueGen :> es) => Eff es (Statement, [Definition])
ex2 = runWriter @[Definition] do
  mult2 <- def "mult2" ["l"] ["a"] \mult2 [l] [a] -> do
    match
      (var l)
      [ branch "Nil" [] [] \_ _ -> literal (Int 1),
        branch "Cons" ["x", "xs"] [] \[x, xs] _ ->
          switch
            (var x)
            [(Int 0, goto (literal (Int 0)) (label a))]
            (prim "mul" [var x, invoke mult2 [var xs] [label a]])
      ]
  fmult <- def "fmult" ["l"] [] \_ [l] [] -> do
    labelOf "a" \a -> do
      invoke mult2 [var l] [label a]
  main <- def "main" [] [] \_ _ _ -> do
    invoke
      fmult
      [ construct
          "Cons"
          [ literal (Int 2),
            construct
              "Cons"
              [ literal (Int 0),
                construct
                  "Cons"
                  [ literal (Int 3),
                    construct "Nil" [] []
                  ]
                  []
              ]
              []
          ]
          []
      ]
      []
  toplevel (invoke main [] [])
