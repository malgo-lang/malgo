{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
module Language.Malgo.FrontEnd.Lexer
  ( lex
  )
where

import           Language.Malgo.FrontEnd.Loc
import           Language.Malgo.FrontEnd.Token
import           Text.Parsec             hiding ( many
                                                , token
                                                , (<|>)
                                                )
import qualified Text.Parsec.Token             as Tok
import           Universum               hiding ( try )

langDef :: Stream s m Char => Tok.GenLanguageDef s u m
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter <|> char '_'
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.caseSensitive   = True
  , Tok.reservedOpNames = [ "."
                          , "+."
                          , "-."
                          , "*."
                          , "/."
                          , ":"
                          , "="
                          , "+"
                          , "-"
                          , "*"
                          , "/"
                          , "%"
                          , "->"
                          , ";"
                          , "=="
                          , "/="
                          , "<"
                          , ">"
                          , "<="
                          , ">="
                          , "&"
                          , "|"
                          , ","
                          ]
  , Tok.reservedNames   = [ "let"
                          , "in"
                          , "type"
                          , "rec"
                          , "if"
                          , "then"
                          , "else"
                          , "true"
                          , "false"
                          , "forall"
                          ]
  }

tokenParser :: Stream s m Char => Tok.GenTokenParser s u m
tokenParser = Tok.makeTokenParser langDef

keyword :: Stream s m Char => String -> Tag -> ParsecT s u m Tag
keyword word t = reserved word >> return t
  where reserved = Tok.reserved tokenParser

op :: Stream s m Char => String -> Tag -> ParsecT s u m Tag
op sym t = reservedOp sym >> return t
  where reservedOp = Tok.reservedOp tokenParser

tag :: forall s u m . Stream s m Char => ParsecT s u m Tag
tag =
  foldl'
      (\b (word, t) -> b <|> keyword word t)
      (keyword "let" LET)
      [ ("in"    , IN)
      , ("type"  , TYPE)
      , ("rec"   , REC)
      , ("if"    , IF)
      , ("then"  , THEN)
      , ("else"  , ELSE)
      , ("true"  , TRUE)
      , ("false" , FALSE)
      , ("forall", FORALL)
      ]
    <|> (lparen >> return LPAREN)
    <|> (rparen >> return RPAREN)
    <|> (lbrack >> return LBRACK)
    <|> (rbrack >> return RBRACK)
    <|> (lbrace >> return LBRACE)
    <|> (rbrace >> return RBRACE)
    <|> foldl'
          (\b (sym, t) -> b <|> op sym t)
          (op "." DOT)
          [ ("+.", PLUS_DOT)
          , ("-.", MINUS_DOT)
          , ("*.", ASTERISK_DOT)
          , ("/.", SLASH_DOT)
          , (":" , COLON)
          , ("=" , EQUAL)
          , ("+" , PLUS)
          , ("-" , MINUS)
          , ("*" , ASTERISK)
          , ("/" , SLASH)
          , ("%" , PERCENT)
          , ("->", ARROW)
          , (";" , SEMICOLON)
          , ("==", EQ_OP)
          , ("/=", NEQ_OP)
          , ("<" , LT_OP)
          , (">" , GT_OP)
          , ("<=", LE_OP)
          , (">=", GE_OP)
          , ("&" , AND_OP)
          , ("|" , OR_OP)
          , ("," , COMMA)
          ]
    <|> (LID . toText <$> largeIdentifier)
    <|> (ID . toText <$> identifier)
    <|> try (FLOAT <$> float)
    <|> (INT <$> natural)
    <|> (CHAR <$> charLiteral)
    <|> (STRING . toText <$> stringLiteral)
 where
  identLetter     = Tok.identLetter langDef
  reservedNames   = Tok.reservedNames (langDef :: Tok.GenLanguageDef s u m)
  lexeme          = Tok.lexeme tokenParser
  natural         = Tok.natural tokenParser
  float           = Tok.float tokenParser
  charLiteral     = Tok.charLiteral tokenParser
  stringLiteral   = Tok.stringLiteral tokenParser
  symbol          = Tok.symbol tokenParser
  lparen          = symbol "("
  rparen          = symbol ")"
  lbrack          = symbol "["
  rbrack          = symbol "]"
  lbrace          = symbol "{"
  rbrace          = symbol "}"
  identifier      = Tok.identifier tokenParser
  largeIdentifier = lexeme $ try $ do
    x  <- upper
    xs <- many identLetter
    if (x : xs) `elem` reservedNames
      then unexpected ("reserved word " ++ show (x : xs))
      else return $ x : xs

token :: Stream s m Char => ParsecT s u m Token
token = do
  pos1 <- getPosition
  t    <- tag
  pos2 <- getPosition
  return $ Loc
    (SrcSpan (sourceName pos1)
             (sourceLine pos1)
             (sourceColumn pos1)
             (sourceLine pos2)
             (sourceColumn pos2)
    )
    t

lex :: Stream s m Char => SourceName -> s -> m (Either ParseError [Token])
lex = flip runParserT () $ do
  whiteSpace
  toks <- many token
  whiteSpace
  eof
  return toks
  where whiteSpace = Tok.whiteSpace tokenParser
