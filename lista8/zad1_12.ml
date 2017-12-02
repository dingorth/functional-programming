module type PQUEUE =
sig
    type priority
    type 'a t
    
    exception EmptyPQueue
  
    val empty : 'a t
    val insert : 'a t -> priority -> 'a -> 'a t
    val remove : 'a t -> priority * 'a * 'a t
end	

module PQueue : PQUEUE with type priority = int = 
struct

    type priority = int
    type 'a t = (priority * 'a) list

    exception EmptyPQueue

    let empty = []
    let insert q p e = 
        let rec aux = function
            | [] -> [p,e]
            | x::xs when fst x >= p -> (p,e)::x::xs
            | x::xs -> x::aux xs
        in
            aux q
    
    let remove = function 
        | [] -> raise EmptyPQueue
        | x::xs -> fst x, snd x, xs

end

let queue_sort l = 
    let rec aux q = function
        | [] -> q
        | x::xs -> aux (PQueue.insert q x x) xs
    and
    aux' q =
        try  
            match PQueue.remove q with
            | _, e, q' -> e :: aux' q'
        with
            PQueue.EmptyPQueue -> [] 
    
    in 
    aux' (aux PQueue.empty l)


