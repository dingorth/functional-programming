module type EDGE = 
sig
  type t 
  type label
  type vertex

  val compare : t -> t -> bool
  val create : vertex -> label -> vertex -> t
  val label : t -> label
  val src : t -> vertex
  val dst : t -> vertex
end

module type VERTEX =
sig
  type t
  type label

  val equal : t -> t -> bool
  val create : label -> t
  val label : t -> label  
end

module Graph (E : EDGE) (V : VERTEX) : GRAPH =
struct


end