type t =
  | Begin of t * t list
  | Define of string * Types.t * t
  | If of t * t * t
  | Symbol of string
  | Int of int
  | Float of float
  | Cons of t * t
  | Nil
[@@deriving show]
