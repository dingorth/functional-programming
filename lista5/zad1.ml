type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec lfrom n = LCons (n, function () -> lfrom (n+1));;

let ltake n ll = 
    let rec aux = function
        | (0, _) -> []
        | (_, LNil) -> []
        | (n, LCons(x,xf)) -> x::aux(n-1, xf())
    in
        aux (n,ll)
;;

let rec toLazyList = function
    | [] -> LNil
    | x::xs -> LCons(x, function () -> toLazyList xs)
;;

let rec lmap f = function
    | LNil -> LNil
    | LCons(x,xf) -> LCons(f x, function () -> lmap f (xf()) )
;;

let pi_fourth_stream =
    let rec aux denom sgn = 
        LCons (sgn *. 1. /. denom, function () -> aux (denom +. 2.) (-.sgn))
    in
        aux 1. 1.
;;


let sum_prefix n ll =
    let rec aux acc = function 
        | (0, _) -> acc
        | (_, LNil) -> acc
        | (n', LCons(x,xf)) -> aux (acc +. x) (n'-1,xf())
    in
        aux 0. (n,ll)
;;

let get_pi n = 4. *. sum_prefix n pi_fourth_stream;;


let map_3_args f ll = 
    let rec aux = function
        | LNil -> failwith "err"
        | LCons(x,xf) -> match (xf()) with
            | LNil -> failwith "err"
            | LCons(x',xf') -> match (xf'()) with
                | LNil -> failwith "err"
                | LCons(x'',xf'') -> LCons(f x x' x'', function () -> aux (xf()))
    in
        aux ll
;;

let pi_euler_transform n =
    let transformed = 
        map_3_args (fun x y z -> z -. (y-.z)*.2./.(x-.2.*.y+.z)) pi_fourth_stream
    in
        ltake n transformed
;;

(* http://mathworld.wolfram.com/EulerTransform.html *)

type 'a lazy_list = LazyNil | LazyCons of 'a * 'a lazy_list lazy_t;;

let lazy_pi_fourth_stream =
    let rec aux s d = 
        LazyCons( s /. d, lazy (aux (-.s) (d +. 2.)))
    in
        aux 1. 1.
;;

let lazy_take n ll = 
    let rec aux = function
        | (0, _) -> []
        | (_, LazyNil) -> []
        | (n, LazyCons(x,xs)) ->  x :: aux (n-1, Lazy.force xs)
    in
        aux (n,ll)
;;

let lazy_sum_prefix n ll = 
    let rec aux acc = function
        | (0, _) -> acc
        | (_, LazyNil) -> acc
        | (n, LazyCons(x,xs)) -> aux (acc +. x) (n-1, Lazy.force xs)
    in
        aux 0. (n,ll)
;;

let lazy_get_pi n = 4. *. lazy_sum_prefix n lazy_pi_fourth_stream;; 

