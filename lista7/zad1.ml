(* Zdefiniuj operator stałopunktowy fix typu (('a->'b)->'a->'b)->'a->'b, 
który pozwoli na wyznaczanie punktu stałego funkcji typu ('a->'b)->'a->'b, 
a co za tym idzie na definiowanie rekurencyjnych funkcji bez użycia 
konstrukcji let rec. Np. silnię można wyrazić przy użyciu fix następująco: *)

(* fix (fun f -> fun n -> if n = 0 then 1 else n * (f (n-1))) *)

(* Nie używając rekursji (tj. konstrukcji let rec) zdefiniuj funkcję obliczającą
silnię (użyj referencji). W podobny sposób zdefiniuj funkcję fix. *)

(* http://www.cs.cornell.edu/courses/cs3110/2013sp/supplemental/lectures/lec29-fixpoints/lec29.html *)
(* https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus *)
(* landyns knot ?? *)

(* zapetla sie! *)
let rec fix f = f (fix f)
let rec fix f x = f (fix f) x

let fact = let f = ref (fun n -> n) in f := (fun n -> if n = 0 then 1 else n * !f (n-1)); !f

let fix' = let f = ref (fun f' x -> x) in f := (fun f' x -> f' (!f f') x); !f

let fact2 = fix' (fun f -> fun n -> if n = 0 then 1 else n * (f (n-1)))
