(* slabo rosnacy *)
let splitByAscendingPrefix = function
    | []  -> ([],[])
    | [x] -> ([x],[])
    | l   -> let rec aux prev = function
        | []    -> ([],[])
        | y::ys -> let p = aux y ys in
            if y >= prev
            then (y :: fst p, snd p)
            else ([], y::ys)
        in
            aux (List.hd l) l 

let take n l = 
    let rec aux = function
        | (0,_)     -> []
        | (_,[])    -> []
        | (n,x::xs) -> x :: aux (n-1, xs)
    in aux (n,l)
    

let deleteGreater el l = 
    let rec aux acc e = function
        | []    -> []
        | x::xs -> if x <= e then aux (x::acc) e xs else acc @ xs
    in
        aux [] el l
    
let findGreaterAndDelete el l = 
    let rec aux acc e = function
        | []    -> (0,[])
        | x::xs -> if x <= e then aux (x::acc) e xs else (x,acc @ xs)
    in
        aux [] el l

let next_perm = function
    | []  -> []
    | [x] -> [x]
    | l   -> 
        let splitted = splitByAscendingPrefix l in
        let prefix = fst splitted and remainder = snd splitted in
            if remainder == [] 
            then List.rev l
            else
                let pivot = List.hd remainder in
                let p = findGreaterAndDelete pivot prefix in
                let greater = fst p and prefix2 = pivot :: snd p in
                let remainder2 = greater :: List.tl remainder in
                let sortedPrefix2 = List.sort (fun a b -> - (compare a b)) prefix2 in
                sortedPrefix2 @ remainder2
                

let factorial z =
    let rec aux acc n = 
        if n == 0
        then acc
        else aux (acc*n) (n-1)
    in aux 1 z

let permutations l = 
    let z = factorial (List.length l) in
    let rec aux acc n = if n == 1
        then acc
        else let gen = List.hd acc in
            aux (next_perm gen :: acc) (n-1)
    in aux [l] z