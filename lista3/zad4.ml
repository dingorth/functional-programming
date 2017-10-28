let is_square m = 
    let n = List.length m in
    let dims = List.map List.length m in
    List.for_all ((==)n) dims

let nth_column n m = List.fold_right (fun row acc -> List.nth row (n-1) :: acc) m []

let transpose m = 
    let n = List.length m in
    snd @@ List.fold_right (fun row acc -> (fst acc - 1 , nth_column (fst acc) m :: snd acc) ) m (n,[])
    
let transpose' m = 
    List.rev @@ snd @@ List.fold_left (fun acc row -> (fst acc+1 , nth_column (fst acc) m :: snd acc )) (1,[]) m
    
let zip l1 l2 = 
    let rec aux = function
        | (x::xs,y::ys) -> (x,y) :: aux (xs,ys)
        | (_,_)         -> []
    in
        aux (l1,l2)
        
let zipWith f l1 l2 = List.map (fun p -> f (fst p) (snd p) ) (zip l1 l2)


let rec list_sum = function
    | [] -> 0
    | x::xs -> x + list_sum xs

let mult_vec v m = List.map list_sum (List.map (zipWith (fun a b -> a * b) v ) @@ transpose m) 

let mult_matrices v1 v2 = List.fold_right (fun vect acc -> mult_vec vect v2 :: acc ) v1 []