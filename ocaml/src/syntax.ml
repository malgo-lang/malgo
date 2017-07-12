type t =
  | Symbol of string
  | Int of int
  | Float of float
  | Typed of t * Types.t
  | List of t list
[@@deriving show]

let sample1 = List [Symbol "def"; Typed (Symbol "answer", Types.Int); Int 42]
let sample2 = Typed (List [Symbol "if";
                           List [Symbol "=="; Symbol "answer"; Int 42];
                           List [Symbol "quote"; Symbol "yes"];
                           List [Symbol "quote"; Symbol "no"]],
                     Types.Symbol)
