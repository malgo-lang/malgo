type t = string

let counter = ref 0
let genid s =
  incr counter;
  Printf.sprintf "%s.%d" s !counter
