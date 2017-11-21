type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree

(* samefringe : t btree -> t btree -> bool *)
let flatten t = 
    let rec aux = function
        | Node(l,r) -> aux l @ aux r
        | Leaf(v) -> [v]
    in
        aux t

let list_compare l1 l2 = 
    let rec aux = function
        | x::xs, y::ys -> if x == y then aux (xs,ys) else false
        | [], [] -> true
        | _ -> false
    in
        aux (l1,l2)

let samefringe t1 t2 = 
    let t1' = flatten t1 in
    let t2' = flatten t2 in
    list_compare t1' t2'

let test_tree_1 = Node (Node (Leaf 1, Leaf 2),Leaf 3);;
let test_tree_2 = Node (Leaf 1, Node (Leaf 2, Leaf 3));;


(* http://caml.inria.fr/pub/ml-archives/caml-list/2003/12/93c3b2ec873addb64754bf56a09fb0a7.en.html *)
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t

let lappend l1 l2 =
    let rec aux = function
        | LNil, l | l, LNil -> l
        | LCons(a,b), l -> LCons(a, lazy (aux (Lazy.force b, l)) )
    in
        aux (l1,l2)

let rec lflatten = function
    | Leaf(v) -> LCons(v, lazy LNil)
    | Node(l,r) -> lappend (lflatten l) (lflatten r)

let llist_compare l1 l2 =
    let rec aux = function
        | LNil, LNil -> true
        | LCons(x,xs), LCons(y,ys) -> if x == y then aux (Lazy.force xs, Lazy.force ys) else false
        | _ -> false
    in
        aux (l1,l2)

let lsamefringe t1 t2 =
    let t1l = lflatten t1 and t2l = lflatten t2 in
    llist_compare t1l t2l
;;

