module Malgo.Parser.LexerSpec (spec) where

import Data.Text qualified as T
import Malgo.Parser.Lexer
import Malgo.Prelude hiding (Space, lex)
import Test.Hspec
import Text.Megaparsec

spec :: SpecWith ()
spec = parallel do
  traverse_ lexTest testCases

lexTest :: (String, Text, [Symbol]) -> SpecWith ()
lexTest (name, input, folded) = xit name do
  case lex "" input of
    Left err -> expectationFailure (errorBundlePretty err)
    Right actual -> do
      map (.value) actual.unLexStream `shouldBe` folded

testCases :: [(String, Text, [Symbol])]
testCases =
  [ ( "identity function",
      "{x -> x}",
      [ReservedOp LBrace, Ident "x", Space 1, ReservedOp Arrow, Space 1, Ident "x", ReservedOp RBrace]
    ),
    ( "identity function with newlines",
      T.unlines
        [ "{",
          "  x ->",
          "    x",
          "}"
        ],
      [ReservedOp LBrace, IndentStart 2, Ident "x", Space 1, ReservedOp Arrow, IndentStart 4, Ident "x", IndentEnd 4, IndentEnd 2, ReservedOp RBrace, Newlines]
    ),
    ( "record type",
      "{ a: Int, b : Int#}",
      [ReservedOp LBrace, Space 1, Ident "a", ReservedOp Colon, Space 1, Ident "Int", ReservedOp Comma, Space 1, Ident "b", Space 1, ReservedOp Colon, Space 1, Ident "Int#", ReservedOp RBrace]
    ),
    ( "pattern match case 1",
      T.unlines
        [ "{",
          "  Nil -> 0",
          "  Cons x xs -> x + xs",
          "}"
        ],
      concat
        [ [ReservedOp LBrace],
          [IndentStart 2, Ident "Nil", Space 1, ReservedOp Arrow, Space 1, Int32 False 0, IndentEnd 2],
          [IndentStart 2, Ident "Cons", Space 1, Ident "x", Space 1, Ident "xs", Space 1, ReservedOp Arrow, Space 1, Ident "x", Space 1, Operator "+", Space 1, Ident "xs", IndentEnd 2],
          [ReservedOp RBrace, Newlines]
        ]
    ),
    ( "pattern match case 2",
      T.unlines
        [ "=",
          "  {",
          "    Nil -> 0",
          "    Cons x xs -> x + xs",
          "  }"
        ],
      concat
        [ [ReservedOp Equal],
          [IndentStart 2, ReservedOp LBrace],
          [IndentStart 4, Ident "Nil", Space 1, ReservedOp Arrow, Space 1, Int32 False 0, IndentEnd 4],
          [IndentStart 4, Ident "Cons", Space 1, Ident "x", Space 1, Ident "xs", Space 1, ReservedOp Arrow, Space 1, Ident "x", Space 1, Operator "+", Space 1, Ident "xs", IndentEnd 4],
          [ReservedOp RBrace, IndentEnd 2]
        ]
    ),
    ( "pattern match case 3",
      T.unlines
        [ "=",
          "  {",
          "      Nil -> 0",
          "    Cons x xs -> x + xs",
          "  }"
        ],
      concat
        [ [ReservedOp Equal],
          [IndentStart 2, ReservedOp LBrace],
          [IndentStart 6, Ident "Nil", Space 1, ReservedOp Arrow, Space 1, Int32 False 0, IndentEnd 6],
          [Ident "Cons", Space 1, Ident "x", Space 1, Ident "xs", Space 1, ReservedOp Arrow, Space 1, Ident "x", Space 1, Operator "+", Space 1, Ident "xs"],
          [IndentEnd 2, IndentStart 2, ReservedOp RBrace, IndentEnd 2]
        ]
    ),
    ( "non-layout function",
      T.unlines
        [ "def main = {",
          "  (null (Cons True (Cons True Nil)))",
          "    |> { False -> malgo_print_string ok",
          "       | True -> malgo_exit_failure ()",
          "       }",
          "}"
        ],
      concat
        [ [ReservedId Def, Space 1, Ident "main", Space 1, ReservedOp Equal, Space 1, ReservedOp LBrace],
          [IndentStart 2, ReservedOp LParen, Ident "null", Space 1, ReservedOp LParen, Ident "Cons", Space 1, Ident "True", Space 1, ReservedOp LParen, Ident "Cons", Space 1, Ident "True", Space 1, Ident "Nil", ReservedOp RParen, ReservedOp RParen, ReservedOp RParen],
          [IndentStart 4, Operator "|>", Space 1, ReservedOp LBrace, Space 1, Ident "False", Space 1, ReservedOp Arrow, Space 1, Ident "malgo_print_string", Space 1, Ident "ok"],
          [IndentStart 7, ReservedOp Bar, Space 1, Ident "True", Space 1, ReservedOp Arrow, Space 1, Ident "malgo_exit_failure", Space 1, ReservedOp LParen, ReservedOp RParen, IndentEnd 7],
          [IndentStart 7, ReservedOp RBrace, IndentEnd 7],
          [IndentEnd 4, IndentEnd 2, ReservedOp RBrace, Newlines]
        ]
    ),
    ( "module import patterns",
      "{..} {x, A, (+)}",
      [ReservedOp LBrace, ReservedOp DotDot, ReservedOp RBrace, Space 1, ReservedOp LBrace, Ident "x", ReservedOp Comma, Space 1, Ident "A", ReservedOp Comma, Space 1, ReservedOp LParen, Operator "+", ReservedOp RParen, ReservedOp RBrace]
    ),
    ( "empty line",
      "def\n main\n \n  = 0",
      [ReservedId Def, IndentStart 1, Ident "main", Newlines, Space 1, IndentStart 2, ReservedOp Equal, Space 1, Int32 False 0, IndentEnd 2, IndentEnd 1]
    ),
    ( "infix",
      "infixr 2 (<|>)",
      [ReservedId Infixr, Space 1, Int32 False 2, Space 1, ReservedOp LParen, Operator "<|>", ReservedOp RParen]
    ),
    ( "qualified variable",
      "M.x D . y N..N.+",
      [Qualified "M" (Ident "x"), Space 1, Ident "D", Space 1, Operator ".", Space 1, Ident "y", Space 1, Qualified "N" (Operator "."), Qualified "N" (Operator "+")]
    ),
    ( "line comment",
      "-- comment",
      [Comment " comment"]
    ),
    ( "block comment",
      "{- comment -}",
      [Comment " comment "]
    ),
    ( "underscore",
      "{ fst = x, snd = _ }",
      [ReservedOp LBrace, Space 1, Ident "fst", Space 1, ReservedOp Equal, Space 1, Ident "x", ReservedOp Comma, Space 1, Ident "snd", Space 1, ReservedOp Equal, Space 1, Ident "_", Space 1, ReservedOp RBrace]
    ),
    ( "multiple defs",
      "\n  def f = 0\n  def g = 1",
      [IndentStart 2, ReservedId Def, Space 1, Ident "f", Space 1, ReservedOp Equal, Space 1, Int32 False 0, IndentEnd 2, IndentStart 2, ReservedId Def, Space 1, Ident "g", Space 1, ReservedOp Equal, Space 1, Int32 False 1, IndentEnd 2]
    ),
    ( "unboxed literal",
      "1# 2.0# 'c'# \"str\"# 1L# 2.0F#",
      [Int32 True 1, Space 1, Double True 2.0, Space 1, Char True 'c', Space 1, String True "str", Space 1, Int64 True 1, Space 1, Float True 2.0]
    ),
    ( "boxed literal",
      "1 2.0 'c' \"str\" 1L 2.0F",
      [Int32 False 1, Space 1, Double False 2.0, Space 1, Char False 'c', Space 1, String False "str", Space 1, Int64 False 1, Space 1, Float False 2.0]
    )
  ]
