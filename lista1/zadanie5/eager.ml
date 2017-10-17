(* let x = print_string("eager\n") in (fun a -> x);;  *)
let y = [1 / 0] in 10;
(* let z = print_string("eager\n") in 10; *)