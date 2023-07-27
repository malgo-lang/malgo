module Malgo.Parser.Stream (Symbol (..), ReservedId (..), ReservedOp (..), WithPos (..), LexStream (..), tabLength) where

import Data.Data hiding (Infix)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Koriel.Pretty
import Malgo.Prelude hiding (Space, lex)
import Prettyprinter.Render.Text (renderStrict)
import Text.Megaparsec

data Symbol
  = -- | sequence of '\\r' and '\\n'
    Newlines
  | -- | length of spaces (tab is 8 spaces)
    Space Int
  | IndentStart Int
  | IndentEnd Int
  | ReservedId ReservedId
  | ReservedOp ReservedOp
  | -- | qualified identifier
    Qualified
      -- | module name
      Text
      -- | identifier
      Symbol
  | -- | identifier but not operator
    Ident Text
  | -- | operator
    Operator Text
  | Int
      -- | is unboxed
      Bool
      Integer
  | Float
      -- | is unboxed
      Bool
      Double
  | Char
      -- | is unboxed
      Bool
      Char
  | String
      -- | is unboxed
      Bool
      Text
  deriving stock (Eq, Ord, Show)

instance Pretty Symbol where
  pretty Newlines = pretty @Text "<newlines>"
  pretty (Space n) = "<space " <> pretty n <> ">"
  pretty (IndentStart n) = "<indent start " <> pretty n <> ">"
  pretty (IndentEnd n) = "<indent end " <> pretty n <> ">"
  pretty (ReservedId k) = pretty k
  pretty (ReservedOp k) = pretty k
  pretty (Qualified moduleName name) = "<qualified " <> pretty moduleName <> "." <> pretty name <> ">"
  pretty (Ident x) = "<ident " <> pretty x <> ">"
  pretty (Operator x) = "<operator " <> pretty x <> ">"
  pretty (Int False x) = "<int " <> pretty x <> ">"
  pretty (Int True x) = "<int# " <> pretty x <> ">"
  pretty (Float False x) = "<float " <> pretty x <> ">"
  pretty (Float True x) = "<float# " <> pretty x <> ">"
  pretty (Char False x) = "<char " <> pretty x <> ">"
  pretty (Char True x) = "<char# " <> pretty x <> ">"
  pretty (String False x) = "<string " <> pretty x <> ">"
  pretty (String True x) = "<string# " <> pretty x <> ">"

data ReservedId
  = -- | @class@ keyword
    Class
  | -- | @def@ keyword
    Def
  | -- | @data@ keyword
    Data
  | -- | @exists@ keyword
    Exists
  | -- | @forall@ keyword
    Forall
  | -- | @foreign@ keyword
    Foreign
  | -- | @impl@ keyword
    Impl
  | -- | @import@ keyword
    Import
  | -- | @infix@ keyword
    Infix
  | -- | @infixl@ keyword
    Infixl
  | -- | @infixr@ keyword
    Infixr
  | -- | @let@ keyword
    Let
  | -- | @type@ keyword
    Type
  | -- | @module@ keyword
    Module
  | -- | @with@ keyword
    With
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data ReservedOp
  = -- | @=>@ keyword
    DArrow
  | -- | @=@ keyword
    Equal
  | -- | @:@ keyword
    Colon
  | -- | @|@ keyword
    Bar
  | -- | @->@ keyword
    Arrow
  | -- | @;@ keyword
    Semicolon
  | -- | @,@ keyword
    Comma
  | -- | @!@ keyword
    Exclamation
  | -- | @..@ keyword
    DotDot
  | -- | @#|@ keyword
    LHash
  | -- | @|#@ keyword
    RHash
  | -- | '('
    LParen
  | -- | ')'
    RParen
  | -- | '{'
    LBrace
  | -- | '}'
    RBrace
  | -- | '['
    LBracket
  | -- | ']'
    RBracket
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance Pretty ReservedId where
  pretty Class = pretty @Text "class"
  pretty Def = pretty @Text "def"
  pretty Data = pretty @Text "data"
  pretty Exists = pretty @Text "exists"
  pretty Forall = pretty @Text "forall"
  pretty Foreign = pretty @Text "foreign"
  pretty Impl = pretty @Text "impl"
  pretty Import = pretty @Text "import"
  pretty Infix = pretty @Text "infix"
  pretty Infixl = pretty @Text "infixl"
  pretty Infixr = pretty @Text "infixr"
  pretty Let = pretty @Text "let"
  pretty Type = pretty @Text "type"
  pretty Module = pretty @Text "module"
  pretty With = pretty @Text "with"

instance Pretty ReservedOp where
  pretty DArrow = pretty @Text "=>"
  pretty Equal = pretty @Text "="
  pretty Colon = pretty @Text ":"
  pretty Bar = pretty @Text "|"
  pretty Arrow = pretty @Text "->"
  pretty Semicolon = pretty @Text ";"
  pretty Comma = pretty @Text ","
  pretty Exclamation = pretty @Text "!"
  pretty DotDot = pretty @Text ".."
  pretty LHash = pretty @Text "#|"
  pretty RHash = pretty @Text "|#"
  pretty LParen = pretty @Text "("
  pretty RParen = pretty @Text ")"
  pretty LBrace = pretty @Text "{"
  pretty RBrace = pretty @Text "}"
  pretty LBracket = pretty @Text "["
  pretty RBracket = pretty @Text "]"

showSymbol :: Symbol -> String
showSymbol = convertString . renderStrict . layoutCompact . pretty

-- | tab is 8 spaces
tabLength :: Int
tabLength = 8

data WithPos a = WithPos
  { startPos :: SourcePos,
    endPos :: SourcePos,
    length :: Int,
    value :: a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data LexStream = LexStream
  { input :: Text,
    unLexStream :: [WithPos Symbol]
  }
  deriving stock (Show)

-- * Stream instance

instance Stream LexStream where
  type Token LexStream = WithPos Symbol
  type Tokens LexStream = [WithPos Symbol]

  tokenToChunk _ = pure
  {-# INLINE tokenToChunk #-}
  tokensToChunk _ = identity
  {-# INLINE tokensToChunk #-}
  chunkToTokens _ = identity
  {-# INLINE chunkToTokens #-}
  chunkLength _ = length
  {-# INLINE chunkLength #-}
  take1_ (LexStream _ []) = Nothing
  take1_ (LexStream input (x : xs)) =
    Just (x, LexStream (T.drop (tokensLength (Proxy @LexStream) (x :| [])) input) xs)
  {-# INLINEABLE take1_ #-}
  takeN_ n (LexStream input xs)
    | n <= 0 = Just ([], LexStream input xs)
    | null xs = Nothing
    | otherwise =
        let (x, xs') = splitAt n xs
         in case NonEmpty.nonEmpty x of
              Nothing -> Just (x, LexStream input xs')
              Just nex -> Just (x, LexStream (T.drop (tokensLength (Proxy @LexStream) nex) input) xs')
  {-# INLINEABLE takeN_ #-}
  takeWhile_ f (LexStream input xs) =
    let (x, xs') = List.span f xs
     in case NonEmpty.nonEmpty x of
          Nothing -> (x, LexStream input xs')
          Just nex -> (x, LexStream (T.drop (tokensLength (Proxy @LexStream) nex) input) xs')
  {-# INLINEABLE takeWhile_ #-}

instance VisualStream LexStream where
  showTokens _ =
    unwords
      . NonEmpty.toList
      . fmap (showSymbol . (.value))
  {-# INLINE showTokens #-}
  tokensLength _ = sum . fmap (.length)
  {-# INLINE tokensLength #-}

instance TraversableStream LexStream where
  reachOffset o PosState {..} =
    ( Just (convertString $ prefix <> restOfLine),
      PosState
        { pstateInput = LexStream {input = postStr, unLexStream = post},
          pstateOffset = max pstateOffset o,
          pstateSourcePos = newSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = convertString prefix
        }
    )
    where
      prefix = if sameLine then convertString pstateLinePrefix <> preLine else preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> pstateSourcePos
          (x : _) -> x.startPos
      (pre, post) = splitAt (o - pstateOffset) pstateInput.unLexStream
      (preStr, postStr) = T.splitAt tokensConsumed pstateInput.input
      preLine = T.reverse . T.takeWhile (/= '\n') . T.reverse $ preStr -- TODO: Use takeWhileEnd
      tokensConsumed =
        case NonEmpty.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength (Proxy @LexStream) nePre
      restOfLine = T.takeWhile (/= '\n') postStr