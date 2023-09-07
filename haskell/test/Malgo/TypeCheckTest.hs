{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Malgo.TypeCheckTest where

import Control.Monad.Except (runExceptT)
import Data.String.Conversions (convertString)
import Malgo.Monad
import Malgo.Parser (parse)
import Malgo.Prelude
import Malgo.Rename (rename)
import Malgo.TypeCheck (typeCheck)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

test_typecheck :: [TestTree]
test_typecheck =
  map
    aux
    [ ("identity", "{ # x -> x }"),
      ("const", "{ # x y -> x }"),
      ("shadowing", "{ # x -> { # x -> x } }")
    ]
  where
    aux (name, input) = goldenVsString ("typeCheck " <> name) ("test/Malgo/TypeCheck/golden/" <> name <> ".golden") do
      case parse "test.mlg" input of
        Left err -> pure $ convertString err
        Right result -> do
          ctx <- newCtx "test.mlg"
          result' <- runMalgoM ctx $ runExceptT do
            renamed <- rename result
            typeCheck renamed
          pure $
            convertString $
              renderStrict $
                layoutSmart defaultLayoutOptions $ case result' of
                  Left err -> err
                  Right result'' -> pretty result''