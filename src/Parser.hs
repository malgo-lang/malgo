module Parser where

import           Control.Applicative   hiding ((<|>))
import           Data.Functor.Identity (Identity)
import qualified Data.Text             as T
import           Syntax
import           Text.Parsec
import           Text.Parsec.Expr
import qualified Text.Parsec.Language  as Lang
import           Text.Parsec.Text
import qualified Text.Parsec.Token     as Tok

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef { Tok.commentStart = "#|"
                      , Tok.commentEnd = "|#"
                      , Tok.commentLine = ";"
                      , Tok.opStart = Tok.opLetter style
                      , Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
                      , Tok.identStart = letter <|> oneOf "-+/*=|&><"
                      , Tok.identLetter = digit <|> letter <|> oneOf "?+=|&-/"
                      , Tok.reservedOpNames = [ ":" ]
                      , Tok.reservedNames = ["define", "let"]
                      }

Tok.TokenParser { Tok.parens = m_parens
                , Tok.identifier = m_identifier
                } = Tok.makeTokenParser style

reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer $ T.unpack op
