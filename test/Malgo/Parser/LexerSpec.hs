module Malgo.Parser.LexerSpec (spec) where

import Data.Text qualified as T
import Malgo.Parser.Lexer
import Malgo.Prelude hiding (Space, lex)
import Test.Hspec
import Text.Megaparsec

spec :: SpecWith ()
spec = parallel do
  traverse_ lexTest testCases

lexTest :: (String, Text, [Symbol], [Symbol]) -> SpecWith ()
lexTest (name, input, expected, folded) = it name do
  case lex "" input of
    Left err -> expectationFailure (errorBundlePretty err)
    Right actual -> do
      map (.value) actual.unLexStream `shouldBe` expected
      map (.value) (foldIndent actual.unLexStream) `shouldBe` folded

testCases :: [(String, Text, [Symbol], [Symbol])]
testCases =
  [ ( "identity function",
      "{x -> x}",
      [ReservedOp LBrace, Ident "x", Space 1, ReservedOp Arrow, Space 1, Ident "x", ReservedOp RBrace],
      [ReservedOp LBrace, Ident "x", ReservedOp Arrow, Ident "x", ReservedOp RBrace]
    ),
    ( "identity function with newlines",
      T.unlines
        [ "{",
          "  x ->",
          "    x",
          "}"
        ],
      [ReservedOp LBrace, Newlines, Space 2, Ident "x", Space 1, ReservedOp Arrow, Newlines, Space 4, Ident "x", Newlines, ReservedOp RBrace, Newlines],
      [ReservedOp LBrace, IndentStart 2, Ident "x", ReservedOp Arrow, IndentStart 4, Ident "x", IndentEnd 4, IndentEnd 2, ReservedOp RBrace]
    ),
    ( "record type",
      "{ a: Int, b : Int#}",
      [ReservedOp LBrace, Space 1, Ident "a", ReservedOp Colon, Space 1, Ident "Int", ReservedOp Comma, Space 1, Ident "b", Space 1, ReservedOp Colon, Space 1, Ident "Int#", ReservedOp RBrace],
      [ReservedOp LBrace, Ident "a", ReservedOp Colon, Ident "Int", ReservedOp Comma, Ident "b", ReservedOp Colon, Ident "Int#", ReservedOp RBrace]
    ),
    ( "pattern match case 1",
      T.unlines
        [ "{",
          "  Nil -> 0",
          "  Cons x xs -> x + xs",
          "}"
        ],
      concat
        [ [ReservedOp LBrace, Newlines],
          [Space 2, Ident "Nil", Space 1, ReservedOp Arrow, Space 1, Int False 0, Newlines],
          [Space 2, Ident "Cons", Space 1, Ident "x", Space 1, Ident "xs", Space 1, ReservedOp Arrow, Space 1, Ident "x", Space 1, Operator "+", Space 1, Ident "xs", Newlines],
          [ReservedOp RBrace, Newlines]
        ],
      concat
        [ [ReservedOp LBrace],
          [IndentStart 2, Ident "Nil", ReservedOp Arrow, Int False 0, IndentEnd 2],
          [IndentStart 2, Ident "Cons", Ident "x", Ident "xs", ReservedOp Arrow, Ident "x", Operator "+", Ident "xs", IndentEnd 2],
          [ReservedOp RBrace]
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
        [ [ReservedOp Equal, Newlines],
          [Space 2, ReservedOp LBrace, Newlines],
          [Space 4, Ident "Nil", Space 1, ReservedOp Arrow, Space 1, Int False 0, Newlines],
          [Space 4, Ident "Cons", Space 1, Ident "x", Space 1, Ident "xs", Space 1, ReservedOp Arrow, Space 1, Ident "x", Space 1, Operator "+", Space 1, Ident "xs", Newlines],
          [Space 2, ReservedOp RBrace, Newlines]
        ],
      concat
        [ [ReservedOp Equal],
          [IndentStart 2, ReservedOp LBrace],
          [IndentStart 4, Ident "Nil", ReservedOp Arrow, Int False 0, IndentEnd 4],
          [IndentStart 4, Ident "Cons", Ident "x", Ident "xs", ReservedOp Arrow, Ident "x", Operator "+", Ident "xs", IndentEnd 4],
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
        [ [ReservedOp Equal, Newlines],
          [Space 2, ReservedOp LBrace, Newlines],
          [Space 6, Ident "Nil", Space 1, ReservedOp Arrow, Space 1, Int False 0, Newlines],
          [Space 4, Ident "Cons", Space 1, Ident "x", Space 1, Ident "xs", Space 1, ReservedOp Arrow, Space 1, Ident "x", Space 1, Operator "+", Space 1, Ident "xs", Newlines],
          [Space 2, ReservedOp RBrace, Newlines]
        ],
      concat
        [ [ReservedOp Equal],
          [IndentStart 2, ReservedOp LBrace],
          [IndentStart 6, Ident "Nil", ReservedOp Arrow, Int False 0, IndentEnd 6],
          [Ident "Cons", Ident "x", Ident "xs", ReservedOp Arrow, Ident "x", Operator "+", Ident "xs"],
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
        [ [ReservedId Def, Space 1, Ident "main", Space 1, ReservedOp Equal, Space 1, ReservedOp LBrace, Newlines],
          [Space 2, ReservedOp LParen, Ident "null", Space 1, ReservedOp LParen, Ident "Cons", Space 1, Ident "True", Space 1, ReservedOp LParen, Ident "Cons", Space 1, Ident "True", Space 1, Ident "Nil", ReservedOp RParen, ReservedOp RParen, ReservedOp RParen, Newlines],
          [Space 4, Operator "|>", Space 1, ReservedOp LBrace, Space 1, Ident "False", Space 1, ReservedOp Arrow, Space 1, Ident "malgo_print_string", Space 1, Ident "ok", Newlines],
          [Space 7, ReservedOp Bar, Space 1, Ident "True", Space 1, ReservedOp Arrow, Space 1, Ident "malgo_exit_failure", Space 1, ReservedOp LParen, ReservedOp RParen, Newlines],
          [Space 7, ReservedOp RBrace, Newlines, ReservedOp RBrace, Newlines]
        ],
      concat
        [ [ReservedId Def, Ident "main", ReservedOp Equal, ReservedOp LBrace],
          [IndentStart 2, ReservedOp LParen, Ident "null", ReservedOp LParen, Ident "Cons", Ident "True", ReservedOp LParen, Ident "Cons", Ident "True", Ident "Nil", ReservedOp RParen, ReservedOp RParen, ReservedOp RParen],
          [IndentStart 4, Operator "|>", ReservedOp LBrace, Ident "False", ReservedOp Arrow, Ident "malgo_print_string", Ident "ok"],
          [IndentStart 7, ReservedOp Bar, Ident "True", ReservedOp Arrow, Ident "malgo_exit_failure", ReservedOp LParen, ReservedOp RParen, IndentEnd 7],
          [IndentStart 7, ReservedOp RBrace, IndentEnd 7],
          [IndentEnd 4, IndentEnd 2, ReservedOp RBrace]
        ]
    )
  ]
