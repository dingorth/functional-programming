type 'a btree = Leaf of 'a | Node of 'a btree * 'a * 'a btree

let test_tree1 = Node (Node (Leaf 'a', 'b', Leaf 'c'), 'd', Leaf 'e')
(* Node (Node (Leaf 3, 2, Leaf 4), 1, Leaf 5). *)

(* VLR – pre-order, przejście wzdłużne *)
let enumerate_tree_VLR t = 
    let rec aux v = function
        | Leaf(_) -> Leaf(v), (v+1)
        | Node(l,_,r) -> 
            let left = aux (v+1) l in 
            let right = aux (snd left) r in
                Node(fst left,v,fst right), snd right
    in
        fst @@ aux 1 t
;;

let test_tree2 = Node (Node (Leaf 'a', 'b', Leaf 'c'), 'd', Leaf 'e')
(* Node (Node (Leaf 4, 2, Leaf 5), 1, Leaf 3). *)

type 'a btree_f = Nil | Nod of 'a * 'a btree_f list;;

let rec to_forest_rep = function
    | Leaf(v) -> Nod(v,[])
    | Node(l,v,r) -> Nod(v,[to_forest_rep l; to_forest_rep r])
;;


(* BFS enumerate *)



