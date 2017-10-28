let horner1 x l = 
    let rec aux acc x = function
        | []    -> acc
        | y::ys -> aux (acc *. x +. y) x ys
    in
        aux 0. x l

let horner2 x l = List.fold_left (fun a b -> a *. x +. b) 0. l