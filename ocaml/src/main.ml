type point2d = float * float
[@@deriving show]

let main () =
  Printf.printf "Hello, world!\n";
  Printf.printf "%s\n" (Id.genid "hoge");
  print_string (show_point2d (2.0, 3.0))

let () = main ()
