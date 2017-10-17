let plus1 = fun x y -> x + y;;
let plus2 x y = x + y;;
let plus3 = function x ->(function y -> x + y);;

let plus_3 = plus1 3;;