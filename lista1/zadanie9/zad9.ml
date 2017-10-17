let testStream = fun x -> x;;
let rec fibStream n = 
    if n < 2 
    then n 
    else (fibStream (n-1)) + fibStream (n-2);;

let hd s = s 0;;
let tl s = fun n -> s (n+1);;
let add x s = fun n -> s n + x;;
let map f s = fun n -> f (s n);;

(* zipWith *)
let map2 f x1 x2 = fun n -> f (x1 n) (x2 n);;
let replace n a s = fun n' -> 
    if n' mod n == 0 
    then a 
    else s n';;

let take n s = fun n' -> s (n' * n);;

let rec helper f a s n = 
    match n with
        | 0 -> f a (s 0)
        | _ -> f (helper f a s (n-1)) @@ s n

let fold f a s = fun n -> helper f a s n;;

(* przerobic na ocamla *)
let rec tabulate = fun a b s =
    | 0             -> []
    | _ when a == b -> [s b]
    | _             -> (s a) :: tabulate (a+1) b s 

let rec tabulate ?(a=0) b s = 
    if a > b 
    then [] 
    else 
        if a == b 
        then [s b] 
        else (s a) :: (tabulate ~a:(a+1) b s);;

let printIntStream n s = 
    for i = 0 to n do
        print_int (s i);
        print_string " ";
done;;