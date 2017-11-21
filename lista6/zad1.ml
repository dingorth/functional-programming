type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree

(* samefringe : t btree -> t btree -> bool *)
let flatten t = 
    let rec aux = function
        | Node(l,r) -> aux l @ aux r
        | Leaf(v) -> [v]
    in
        aux t
;;

let list_compare l1 l2 = 
    let rec aux = function
        | x::xs, y::ys -> if x == y then aux (xs,ys) else false
        | [], [] -> true
        | _ -> false
    in
        aux (l1,l2)
;;

let samefringe t1 t2 = 
    let t1' = flatten t1 in
    let t2' = flatten t2 in
    list_compare t1' t2'
;;

let test_tree_1 = Node (Node (Leaf 1, Leaf 2),Leaf 3);;
let test_tree_2 = Node (Leaf 1, Node (Leaf 2, Leaf 3));;


(* http://caml.inria.fr/pub/ml-archives/caml-list/2003/12/93c3b2ec873addb64754bf56a09fb0a7.en.html *)