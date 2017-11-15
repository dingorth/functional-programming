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

let lNth n ll = 
    let rec aux = function
        | (0, LCons(x,_)) -> x
        | (n, LCons(x,xf)) -> aux (n-1, xf())
        | _ -> failwith "end of list" 
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
        LCons (sgn /. denom, function () -> aux (denom +. 2.) (-.sgn))
    in
        aux 1. 1.
;;

let pi_fourth_sums_stream = 
    let rec aux denom sgn p =
        let v = sgn /. denom in
        LCons(v +. p, function () -> aux (denom +. 2.) (-.sgn) (v +. p))
    in 
        aux 1. 1. 0.
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

let prefix_sums ll =
    let rec aux acc = function
        | LCons(x,xf) -> LCons(acc +. x, function () -> aux (acc +. x) (xf()) )
    in
        aux 0. ll
;;

(* F x y z = z - (y-z)2/(x-2y+z) *)
let euler_function x y z = z -. (y -. z) *. (y -. z) /. (x -. 2. *. y +. z);;

let get_faster_pi n = 4. *. (lNth n @@ map_3_args euler_function pi_fourth_sums_stream);;





(* lazy construct *)
type 'a lazy_list = LazyNil | LazyCons of 'a * 'a lazy_list lazy_t;;

let lazy_pi_fourth_stream =
    let rec aux s d = 
        LazyCons( s /. d, lazy (aux (-.s) (d +. 2.)))
    in
        aux 1. 1.
;;

let lazy_pi_fourth_sums_stream = 
    let rec aux s d p = 
        let v = s /. d in
        LazyCons(v +. p, lazy (aux (-.s) (d +. 2.) (v +. p)))
    in
        aux 1. 1. 0.
;;

let lazy_take n ll = 
    let rec aux = function
        | (0, _) -> []
        | (_, LazyNil) -> []
        | (n, LazyCons(x,xs)) ->  x :: aux (n-1, Lazy.force xs)
    in
        aux (n,ll)
;;

let lazy_Nth n ll = 
    let rec aux = function
        | (0, LazyCons(x,xs)) -> x
        | (n, LazyCons(x,xs)) -> aux (n-1, Lazy.force xs)
        | _ -> failwith "end of list"
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

let lazy_get_pi n = 4. *. (lazy_Nth n lazy_pi_fourth_sums_stream);;

let lazy_map_3_args f ll = 
    let rec aux = function
        | LazyNil -> failwith "err"
        | LazyCons(x,xs) -> match Lazy.force xs with
            | LazyNil -> failwith "err"
            | LazyCons(x',xs') -> match Lazy.force xs' with
                | LazyNil -> failwith "err"
                | LazyCons(x'',xs'') -> LazyCons(f x x' x'', lazy (aux @@ Lazy.force xs))
    in
        aux ll
;;

let rec cos k = LazyCons(k, lazy (cos (k+1)));;

let lazy_get_faster_pi n = 4. *. (lazy_Nth n @@ lazy_map_3_args euler_function lazy_pi_fourth_sums_stream);;