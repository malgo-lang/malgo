{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.Malgo.Backend.Environment where

import Language.Malgo.Prelude
import Language.Malgo.Monad
import Language.Malgo.Type
import Language.Malgo.Backend.MIR

-- | 標準関数
prelude :: [(Text, Type)]
prelude = [ ("print", FunTy ["String"] "Unit")
          , ("println", FunTy ["String"] "Unit")
          , ("intToString", FunTy ["Int"] "String")
          , ("floatToString", FunTy ["Float"] "String")
          , ("boolToString", FunTy ["Bool"] "String")
          ]
