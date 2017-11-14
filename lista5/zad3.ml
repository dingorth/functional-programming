type 'a leftist =  
  | Leaf 
  | Node of 'a leftist * 'a * 'a leftist * int

let singleton k = Node (Leaf, k, Leaf, 1)

let rank = function 
    | Leaf -> 0 
    | Node (_,_,_,r) -> r

let is_singleton = function
  | Node(Leaf,_,Leaf,_) -> true
  | _ -> false

let rec merge cmp t1 t2 =  
  match t1,t2 with
    | Leaf, t | t, Leaf -> t
    | Node (l, k1, r, _), Node (_, k2, _, _) ->
      if cmp k1 k2 then merge cmp t2 t1 (* switch merge if necessary *)
      else 
        let merged = merge cmp r t2 in (* always merge with right *)
        let rank_left = rank l and rank_right = rank merged in
        if rank_left >= rank_right then Node (l, k1, merged, rank_right+1)
        else Node (merged, k1, l, rank_left+1) (* left becomes right due to being shorter *)

let insert cmp x t = merge cmp (singleton x) t

let get_min = function  
  | Leaf -> failwith "empty"
  | Node (_, k, _, _) -> k

let delete_min cmp = function  
  | Leaf -> failwith "empty"
  | Node (l, _, r, _) -> merge cmp l r

let make_heap cmp l = List.fold_left (fun a b -> insert cmp b a) Leaf l;;

(* Drzewo kodowe jest drzewem binarnym, w którego liściach
znajdują się symbole wraz z ich częstościami, a w każdym węźle 
znajduje się suma częstości poddrzew lewego i prawego. Zdefiniuj 
typ takich drzew htree. *)

type htree = Leaf of char * int | Node of htree * int * htree;;

(* 
Algorytm budowy drzewa Huffmana jest następujący:

- Mając daną listę drzew, wybieramy z niej dwa drzewa o najmniejszych częstościach 
(ten wybór nie musi być jednoznaczny).
- Usuwamy te drzewa z listy, łączymy je w jedno drzewo i wstawiamy do listy.
- Kontynuujemy aż do otrzymania listy jednoelementowej (czyli drzewa kodowego).
- Początkowa lista składa się z liści otrzymanych z listy częstości.
- Łączenie dwóch poddrzew polega na utworzeniu nowego drzewa o częstości będącej sumą częstości obu poddrzew.
*)
let tree_weight = function Leaf(_,x) | Node(_,x,_) -> x;;

let join_trees l r = Node(l, tree_weight l + tree_weight r, r);;

let test_list = [('a',45); ('b',13); ('c',12); ('d',16); ('e',9); ('f',5)];;

let make_single_ht p = Leaf(fst p, snd p);; 

(* Zaimplementuj funkcję mkHTree : (char * int) list -> htree implementującą ten algorytm. *)
let hcmp a b = tree_weight a > tree_weight b;;

let mkHTree l = 
  let ht_list = List.map make_single_ht l in
  let ht_heap = make_heap hcmp ht_list in
  let rec aux h = 
    if is_singleton h
    then get_min h
    else 
      let m = get_min h and d = delete_min hcmp h in
      let m' = get_min d and d' = delete_min hcmp d in
        aux @@ insert hcmp (join_trees m m') d'
  in
    aux ht_heap
;;

(* encode : htree -> char stream -> char stream *)

