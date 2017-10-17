let rec fib n =
          if n < 2 then 1 else (fib (n-1)) + (fib (n-2));;

for i = 1 to 10 do
  print_int (fib 30);
    print_newline ();
done;;

(* ocamlc vs ocamlopt *)

(* 
ocamlc compiles to bytecode for ZINC machine. It's not native code. 
It's f.e. using stack to pass arguments to functions.
So, fib_ocamlc is slower than fib_ocamlopt which is compiled to native code. 
*)
