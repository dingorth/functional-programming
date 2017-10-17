let a = if true then 4 else 5;;
let b = if false then 1 else 3.5;;  (*wrong*)
let c = 4.75 + 2.34;;               (*wrong*)
let d = false || "ab">"cd";;
let e = if true then ();;           
let f = if false then () else 4;;   (*wrong*)
let x = 2 in x^"aa";;               (*wrong*)
let y = "abc" in y^y;;              
let z = (fun x -> x.[1]) "abcdef";; 
let g = (fun x -> x) true;;
let h = [1;2] in x@x
let rec f f = f+f in f 42

let i = if false then ();; (* type unit *)