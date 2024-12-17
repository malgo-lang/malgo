{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Malgo.Core
  ( Producer (..),
    Copattern,
    Literal (..),
    Consumer (..),
    Pattern,
    Statement (..),
    Definition (..),
    ex1,
    ex2,
  )
where

import Control.Lens.Indexed (ifor)
import Data.Traversable (for)
import Effectful.Writer.Static.Local (Writer, execWriter, tell)
import GHC.Stack (HasCallStack)
import Malgo.Location
import Malgo.Name
import Malgo.Prelude
import Malgo.Unique (UniqueGen)

-- | @Producer@ represents a term that produces values
data Producer
  = Var Location Name
  | Literal Location Literal
  | Do Location Name Statement
  | Construct Location Text [Producer] [Consumer]
  | Comatch Location [(Copattern, Statement)]
  deriving (Show, Eq)

instance HasLocation Producer where
  location (Var loc _) = loc
  location (Literal loc _) = loc
  location (Do loc _ _) = loc
  location (Construct loc _ _ _) = loc
  location (Comatch loc _) = loc

type Copattern = (Text, [Name], [Name])

-- | @Const@ represents a constant value
data Literal = Int Int
  deriving (Show, Eq)

-- | @Consumer@ represents a term that consumes values
data Consumer
  = Finish Location
  | Label Location Name
  | Then Location Name Statement
  | Destruct Location Text [Producer] [Consumer]
  | Match Location [(Pattern, Statement)]
  deriving (Show, Eq)

instance HasLocation Consumer where
  location (Finish loc) = loc
  location (Label loc _) = loc
  location (Then loc _ _) = loc
  location (Destruct loc _ _ _) = loc
  location (Match loc _) = loc

type Pattern = (Text, [Name], [Name])

-- | @Statement@ represents a statement
data Statement
  = Prim Location Text [Producer] Consumer
  | Switch Location Producer [(Literal, Statement)] Statement
  | Cut Location Producer Consumer
  | Invoke Location Name [Producer] [Consumer]
  deriving (Show, Eq)

instance HasLocation Statement where
  location (Prim loc _ _ _) = loc
  location (Switch loc _ _ _) = loc
  location (Cut loc _ _) = loc
  location (Invoke loc _ _ _) = loc

-- | @Definition@ represents a top-level definition
data Definition = Definition
  { name :: Name,
    params :: [Name],
    returns :: [Name],
    body :: Statement
  }
  deriving (Show)

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
  tell [Definition name' params' returns' (Cut fromCallStack body' (Label fromCallStack ret))]
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

ex1 :: (UniqueGen :> es) => Eff es [Definition]
ex1 = execWriter @[Definition] do
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
  _ <- def "monus" ["n", "m"] [] \monus [n, m] [] -> do
    switch
      (var m)
      [(Int 0, var n)]
      $ switch
        (var n)
        [(Int 0, literal $ Int 0)]
      $ invoke monus [prim "sub" [var n, literal $ Int 1], prim "sub" [var m, literal $ Int 1]] []
  pure ()

ex2 :: (UniqueGen :> es) => Eff es [Definition]
ex2 = execWriter @[Definition] do
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
  _ <- def "main" [] [] \_ _ _ -> do
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
  pure ()
