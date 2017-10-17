let rec foo f g = fun x -> f (g x);;

let rec iterate f n = 
    if n == 0 
    then (fun x-> x)
    else foo (iterate f (n-1)) f

let iterateMatch = 
    match n with
    | 0 -> (fun x -> x)
    | _ -> foo (iterate f (n-1)) f 


let rec iterateAcc ?(acc=(fun x -> x)) f n =
    if n == 0
    then acc
    else iterateAcc ~acc:(foo f acc) f (n-1)

let multiply x y = 
    let a = (+) y in iterate a x 0;;

let power x y = iterate (multiply x) y 1;;
