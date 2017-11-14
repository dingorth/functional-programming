type 'a leftist =  
  | Leaf 
  | Node of 'a leftist * 'a * 'a leftist * int

let singleton k = Node (Leaf, k, Leaf, 1)

let rank = function 
    | Leaf -> 0 
    | Node (_,_,_,r) -> r 

let rec merge cmp t1 t2 =  
  match t1,t2 with
    | Leaf, t | t, Leaf -> t
    | Node (l, k1, r, _), Node (_, k2, _, _) ->
      if (*k1 > k2*) cmp k1 k2 then merge cmp t2 t1 (* switch merge if necessary *)
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