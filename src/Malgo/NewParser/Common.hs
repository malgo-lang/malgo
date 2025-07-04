module Malgo.NewParser.Common
  ( Parser,
    optional,
    manyUnaryOp,
  )
where

import Control.Applicative (Alternative, some)
import Data.Text.Lazy qualified as TL
import Effectful (Eff)
import Malgo.Prelude
import Text.Megaparsec (ParsecT, try, (<|>))

type Parser es = ParsecT Void TL.Text (Eff es)

optional :: Parser es a -> Parser es (Maybe a)
optional p = try (fmap Just p) <|> pure Nothing

manyUnaryOp :: (Alternative f) => f (c -> c) -> f (c -> c)
manyUnaryOp singleUnaryOp = foldr1 (>>>) <$> some singleUnaryOp