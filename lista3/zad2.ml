let horner3 x l = 
    let rec aux x = function
        | []    -> 0.
        | y::ys -> y +. x *. (aux x ys)
    in
        aux x l

let horner4 x l = List.fold_right (fun a b -> a +. b *. x) l 0.