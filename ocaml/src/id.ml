type t = string

let counter = ref 0
let genid s =
  incr counter
