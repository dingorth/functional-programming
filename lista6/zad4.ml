(* Chcemy w Ocamlu zdefiniować funkcję sprintf znaną z języka C, tak by np. *)
(* sprintf "Ala ma %d kot%s." : int -> string -> string *)
(* pozwalało zdefiniować funkcję *)
(* fun n -> sprintf "Ala ma %d kot%s." n (if n = 1 then "a" else if 1 < n & n < 5 then "y" else "ów") *)
(*
lit s - stała napisowa s    string -> (string -> 'a) -> string -> (string -> 'a) 
eol - koniec wiersza        (string -> 'a) -> string -> 'a
inr - liczba typu int       (string -> 'a) -> string -> (int -> 'a)
flt - liczba typu float     (string -> 'a) -> string -> (float -> 'a)
str - napis typu string     (string -> 'a) -> string -> (string -> 'a)
*)
(* sprintf (lit "Ala ma " ++ inr ++ lit " kot" ++ str ++ lit ".") : int -> string -> string *)

(* dyrektywy powinny być funkcjami transformującymi kontynuacje, a operator ++ to 
zwyczajne złożenie takich funkcji. 
Na przykład inr powinien mieć typ 
(string -> 'a) -> string -> (int -> 'a) 
(argumentem ma być kontynuacja oczekująca napisu, ale o nieokreślonym typie odpowiedzi, 
a wynikiem ma być kontynuacja oczekująca napisu, a następnie liczby całkowitej). 
Podobnie, typem eol będzie (string -> a) -> string -> a. *)

let (++) f g =  fun x -> f ( g x )
let sprintf k = k (fun v -> v) ""

let lit s k = fun str -> (k (str ^ s))
let eol k = fun str -> (k (str ^ "\n"))
let inr k = fun str -> (fun n -> k (str ^ string_of_int n))
let flt k = fun str -> (fun f -> k (str ^ string_of_float f))
let str k = fun str -> (fun s -> k (str ^ s))

let koty n = sprintf (lit "Ala ma " ++ inr ++ lit " kot" ++ str ++ lit "." ++ eol) n (if n == 1 then "a" else (if 1 < n && n < 5 then "y" else "ow"));;
