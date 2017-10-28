let derivative1 l = 
    let rec aux n = function
        | []    -> []
        | x::xs -> x *. n :: (aux (n +. 1.) xs)
    in if l == [] then [] else aux 1. (List.tl l)
    
(* inaczej rozwiązane sprawdzenie listy pustej | zeby nie bylo bledu przy braniu glowy listy pustej *)
let derivative1' = function
    | []    -> []
    | _::xs -> let rec aux n = function
        | []    -> []
        | y::ys -> y *. n :: (aux (n +. 1.) ys)
        in aux 1. xs

(* fold_left needs List.rev *)
let derivative2 = function
    | []    -> []
    | _::xs -> let aux a b = 
        let n = fst a and acc = snd a in
        (n +. 1., b *. n :: acc)
    in
        List.rev @@ snd @@ List.fold_left aux (1.0 , []) xs
        
(* fold_right needs List.length *)
let derivative2' = function
    | []    -> []
    | _::xs -> let aux a b = 
        let n = fst b and acc = snd b in
        (n -. 1., a *. n :: acc)
    in
        snd @@ List.fold_right aux xs (float_of_int @@ List.length xs, [])
 