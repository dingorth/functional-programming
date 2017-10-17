let a = fun x -> x;;
(* - : 'a -> 'a = <fun> *)

let b = fun x -> x + 0;;
let b' = fun (x : int) -> x
(* - : int -> int = <fun> *)

let c fun f g -> ( fun x -> f (g x));;
(* - : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun> *)

let rec d x = d x;;
(* - : 'a -> 'b = <fun> *)

