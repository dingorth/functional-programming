module type VERTEX =
sig
  type t
  type label

  val equal : t -> t -> bool
  val create : label -> t
  val label : t -> label  
end

module IntVertex : VERTEX =
struct
  type label = int
  type t = label

  let equal v1 v2 = v1 == v2
  let create l = l
  let label v = v
end

module type EDGE = 
sig
  type t 
  type label
  type vertex

  val equal : t -> t -> bool
  val create : vertex -> label -> vertex -> t
  val label : t -> label
  val vertices : t -> vertex * vertex
end

module IntVertexEdge : EDGE = 
struct
  type label = string
  type vertex = IntVertex.t
  type t = vertex * label * vertex

  let equal e1 e2 = e1 == e2
  let create v1 l v2 = (v1,l,v2)
  let label e = match e with (_,l,_) -> l
  let vertices e = match e with (v1,_,v2) -> v1,v2
end

module type GRAPH =
  sig
  (* typ reprezentacji grafu *)
  type t

  module V : VERTEX
  type vertex = V.t

  module E : EDGE with type vertex = vertex

  type edge = E.t

  (* funkcje wyszukiwania *)
  val mem_v : t -> vertex -> bool
  val mem_e : t -> edge -> bool
  val mem_e_v : t -> vertex -> vertex -> bool
  val find_e : t -> vertex -> vertex -> edge
  val succ : t -> vertex -> vertex list
  val pred : t -> vertex -> vertex list
  val succ_e : t -> vertex -> edge list
  val pred_e : t -> vertex -> edge list

  (* funkcje modyfikacji *) 
  val empty : t
  val add_e : t -> edge -> t
  val add_v : t -> vertex -> t
  val rem_e : t -> edge -> t
  val rem_v : t -> vertex -> t

  (* iteratory *)
  val fold_v : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
end
