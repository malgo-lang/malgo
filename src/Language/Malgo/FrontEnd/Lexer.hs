{-# LANGUAGE OverloadedStrings #-}
module Language.Malgo.FrontEnd.Lexer where

import           Data.String
import           Language.Malgo.Prelude
import           Text.Parsec            hiding (many, (<|>))
import           Text.Parsec.Language
import qualified Text.Parsec.Token      as Tok

type Lexer u a = ParsecT String u Identity a

getInfo :: Lexer u Info
getInfo = do
  pos <- getPosition
  return (Info (toS $ sourceName pos, sourceLine pos, sourceColumn pos))

tokenParser :: Tok.GenTokenParser String u Identity
tokenParser = Tok.makeTokenParser $
  emptyDef
