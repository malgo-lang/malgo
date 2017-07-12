let main () =
  Printf.printf "Hello, world!\n";
  Printf.printf "%s\n%s\n" (Syntax.show Syntax.sample1) (Syntax.show Syntax.sample2)

let () = main ()
