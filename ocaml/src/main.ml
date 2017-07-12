let main () =
  Printf.printf "Hello, world!\n";
  print_string (Syntax.show (Syntax.List [Syntax.Define; Syntax.Symbol "r"; Syntax.Number 10.]))

let () = main ()
