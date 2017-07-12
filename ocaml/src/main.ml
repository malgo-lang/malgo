let main () =
  Printf.printf "Hello, world!\n";
  print_string (Syntax.show (Syntax.Define ("r", Types.Int, Syntax.Int 10)))

let () = main ()
