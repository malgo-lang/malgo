type t =
  | Begin
  | Define
  | If
  | Symbol of string
  | Number of float
  | List of t list
[@@deriving show]
