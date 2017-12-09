(* Niech n osób stoi w
okręgu oraz niech dana będzie liczba m <= n. Rozpoczynając od wskazanej osoby,
przebiegamy po okręgu, usuwając co m-tą osobę. Po usunięciu każdej kolejnej osoby
odliczanie odbywa się w nowo powstałym okręgu. Proces ten postępuje, aż zostaną
usunięte wszystkie osoby. Porządek, w którym osoby stojące początkowo w okręgu są z
niego usuwane, definiuje permutację Józefa typu (n,m) liczb 1, 2, ... ,n. Na przykład
permutacją Józefa typu (7,3) jest <3,6,2,7,5,1,4>. Napisz funkcję typu int -> int -> int
list, która dla danych n oraz m zwraca listę z permutacja Józefa typu (n,m). Należy
wykorzystać listę cykliczną.  *)

type 'a lnode = {item: 'a; mutable next: 'a lnode};;

let mk_circular_list e =
  let rec x = {item=e; next=x}
  in x

let insert_tail e l =
  let x = {item=e; next=l.next}
  in l.next <- x; x

let insert_head e l =
  let x = {item=e; next=l.next}
  in l.next <- x; l

let make_circular_list_n n = 
  let l = mk_circular_list 1 in
  let rec aux list = function
    | e when e <= n -> aux (insert_tail e list) (e+1)
    | _ -> list
  in
    aux l 2

let delete_first l =
  l.next <- l.next.next; l

let joseph n m =
  let l = make_circular_list_n n in
  let node = ref l in
  while !node.next != !node do
    for i = 1 to m-1 do
      node := !node.next;
    done;
    print_int !node.next.item;
    print_char ' ';
    node := delete_first !node;
  done; print_int !node.item

let single_joseph node m =
  for i = 1 to m-1 do
      node := !node.next;
  done;
  let elem = !node.next.item in
  node := delete_first !node; elem

let joseph_list n m = 
  let l = ref @@ make_circular_list_n n in
  let rec aux = function
    | 0 -> []
    | n' -> single_joseph l m :: aux (n'-1)
  in
    List.rev @@ aux n