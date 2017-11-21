(* Zakładamy przy tym, że drzewo reprezentuje tablicę indeksowaną 
liczbami całkowitymi od 1 do n, a ścieżka do składowej o 
indeksie k, wyznaczona jest przez serię dzieleń modulo 2, aż do 
osiagnięcia wartości 1, wg zasady: jeśli k mod 2 = 0, to wybieramy 
lewego syna, a w przeciwnym razie - prawego, a następnie poszukujemy 
elementu o indeksie k div 2. W przypadku drzew zbalansowanych, a z 
takimi mamy tu do czynienia, dostęp do k-tego elementu wymaga 
log k kroków. *)

(* Zdefiniuj typ danych 'a array 
(wraz z drzewem warto przechowywać najwyższy indeks w tablicy) oraz 
następujące operacje na tablicach funkcyjnych: *)

type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree
type 'a array = Array of 'a btree * int;;

let test_arr1 = Array(Node(Node(Node(Leaf,4,Leaf),2,Node(Leaf,6,Leaf)),1,Node(Node(Leaf,5,Leaf),3,Leaf)),6);;

let get_max_index = function Array(_,n) -> n
let get_val = function Leaf -> failwith "no val in leaf" | Node(_,v,_) -> v
let get_left = function Leaf -> failwith "no left in leaf" | Node(l,_,_) -> l
let get_right = function Leaf -> failwith "no right in leaf" | Node(_,_,r) -> r

(* aempty : 'a array, tablica pusta; *)
let aempty = Array(Leaf,0)

(* asub : 'a array -> int -> 'a, pobranie składowej o zadanym indeksie; *)
let asub a k = match a with Array(tree, max) ->
    if k > max then failwith "out of range"
    else let rec aux t i = 
        if i == 1 then get_val t
        else let next = i mod 2 in match next with
            | 0 -> aux (get_left t) (i/2)
            | _ -> aux (get_right t) (i/2)
    in
        aux tree k
;;

(* aupdate : 'a array -> int -> 'a -> 'a array, modyfikacja składowej o zadanym indeksie; *)
let aupdate a k v = match a with Array(tree, max) ->
    if k > max then failwith "out of range"
    else let rec aux t i = match t with Leaf -> failwith "err" | Node(l,v',r) ->
        if i == 1 then Node(l,v,r)
        else let next = i mod 2 in match next with
            | 0 -> Node(aux l (i/2),v',r)
            | _ -> Node(l,v',aux r (i/2))
    in
        aux tree k
;;

(* ahiext : 'a array -> 'a -> 'a array, rozszerzenie tablicy o jedną składową; *)
let ahiext a v = match a with Array(tree,max) ->
    let rec aux = function
        | Leaf, 1 -> Node(Leaf,v,Leaf)
        | Node(l,v',r), i -> (match i mod 2 with
            | 0 -> Node(aux (l,(i/2)),v',r) 
            | _ -> Node(l,v',aux (r,(i/2))))
        | _ -> failwith "err"
    in
        Array(aux (tree,max + 1), max + 1)
;;

(* ahirem : 'a array -> 'a array, usunięcie składowej o najwyższym indeksie. *)
let ahirem a = match a with Array(tree,max) ->
    let rec aux = function
        | Node(_,_,_), 1 -> Leaf
        | Node(l,v',r), i -> (match i mod 2 with
            | 0 -> Node(aux (l,(i/2)),v',r) 
            | _ -> Node(l,v',aux (r,(i/2))))
        | _ -> failwith "err"
    in
        Array(aux (tree,max), max - 1)
;;