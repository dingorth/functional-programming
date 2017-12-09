type 'a list_mutable = LMnil | LMcons of 'a * 'a list_mutable ref

(* Zaimplementuj konkatenację list typu 'a list_mutable na dwa sposoby:
funkcja concat_copy buduje listę wynikową kopiując pierwszy argument;
funkcja concat_share buduje listę wynikową bez kopiowania argumentów. *)

let concat_copy l r = 
    let rec aux = function
        | LMnil, x | x, LMnil -> x
        | LMcons(x,xs), ys -> LMcons(x, ref @@ aux (!xs, ys) )
    in
        aux (l,r)

let concat_share l r =
    let rec aux = function
        | LMcons(x,xs), ys -> 
            if !xs == LMnil 
            then  (xs := ys; l)
            else aux (!xs,ys)
        | LMnil, ys -> ys
    in
        aux (l,r)

let concat_share' l r =
    let rec aux = function
        | LMcons(_,({contents = LMnil } as xs)), ys -> xs := ys; l
        | LMcons(_,xs), ys -> aux (!xs,ys)
        | LMnil, ys -> ys
    in
        aux (l,r)