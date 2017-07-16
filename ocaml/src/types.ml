type t =
  | Int
  | Float
  | Symbol

let rec show = function
  | Int -> "Int"
  | Float -> "Float"
  | Symbol -> "Symbol"
