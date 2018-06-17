{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Malgo.MiddleEnd.Environment where

import           Language.Malgo.IR.IR
import           Language.Malgo.Monad
import           Language.Malgo.Prelude

-- | predefined functions
prelude :: (MonadMalgo s m, HasMType name, IsString name, HasMType exp) => m [(name, exp)]
prelude = return
  [ ("add_int", undefined)
  , ("add_float", undefined)
  , ("sub_int", undefined)
  , ("sub_float", undefined)
  , ("mul_int", undefined)
  , ("mul_float", undefined)
  , ("div_int", undefined)
  , ("div_float", undefined)
  , ("eq_int", undefined)
  , ("eq_float", undefined)
  , ("eq_char", undefined)
  , ("eq_string", undefined)]
