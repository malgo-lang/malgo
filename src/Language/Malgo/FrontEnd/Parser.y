-- -*- mode: text -*-
{
{-# LANGUAGE NoStrictData, OverloadedStrings #-}
module Language.Malgo.FrontEnd.Parser (parse) where

import Prelude
import Data.Text (Text)
import Language.Malgo.IR.AST
import Language.Malgo.FrontEnd.Loc
import Language.Malgo.FrontEnd.Token
}

%name parse decs

%tokentype { Token }
%error { parseError }
%errorhandlertype explist

%token
LET { Loc _ LET }

%%

decs :: { [Decl Text] }
decs : { [] }

{
parseError :: ([Token], [String]) -> a
parseError ([], xs) = error $ "Parse error at EOF: " <> show xs <> " are expected."
parseError (t:_, xs) = error $ "Parse error: " <> show t <> " is got, but " <> show xs <> "are expected."
}
