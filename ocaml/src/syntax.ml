type t =
  | Symbol of string
  | Int of int
  | Float of float
  | Bool of bool
  | Typed of t * Types.t
  | List of t list

let sample1 = List [Symbol "def"; Typed (Symbol "answer", Types.Int); Int 42]
let sample2 = Typed (List [Symbol "if";
                           List [Symbol "=="; Symbol "answer"; Int 42];
                           List [Symbol "quote"; Symbol "yes"];
                           List [Symbol "quote"; Symbol "no"]],
                     Types.Symbol)

let rec show = function
  | Symbol sym -> sym
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Typed (e, t) -> show e ^ ":" ^ Types.show t
  | List l -> "(" ^ show_list l ^ ")"
and show_list = function
  | [] -> ""
  | [x] -> show x
  | x :: xs -> show x ^ " " ^ show_list xs
