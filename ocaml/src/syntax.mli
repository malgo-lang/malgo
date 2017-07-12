type t =
    Begin
  | Define
  | If
  | Symbol of string
  | Number of float
  | List of t list
val pp : Format.formatter -> t -> Ppx_deriving_runtime.unit
val show : t -> Ppx_deriving_runtime.string
