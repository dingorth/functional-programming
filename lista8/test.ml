module type VERTEX =
sig
  type t
  type label

  val compare : t -> t -> int
  val create : label -> t
  val label : t -> label  
end

module type EDGE = 
sig
  type t 
  type label
  type vertex

  val compare : t -> t -> int
  val create : vertex -> label -> vertex -> t
  val label : t -> label
  val src : t -> vertex
  val dst : t -> vertex
end

module IntVertex : VERTEX with type label = int = 
struct
  type label = int
  type t = label

  let compare = Pervasives.compare
  let create l = l
  let label v = v
end

module IntVertexEdge : EDGE with type vertex = IntVertex.t =
struct
  type label = string
  type vertex = IntVertex.t
  type t = vertex * label * vertex

  let compare = Pervasives.compare
  let create v1 l v2 = (v1,l,v2)
  let label e = match e with (_,l,_) -> l
  let src = function (v,_,_) -> v
  let dst = function (_,_,v) -> v
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

  (* funkcje modyfikacji *) 
  val empty : t
  val add_e : t -> edge -> t
  val add_v : t -> vertex -> t
end

(* GRAPH with type vertex = Vertex.t and type edge = Edge.t *)
module Graph (Vertex : VERTEX) (Edge : EDGE with type vertex = Vertex.t) : GRAPH =
struct
  module V = Vertex
  type vertex = V.t

  module E = Edge
  type edge = E.t

  module VSet = Set.Make(V)
  module ESet = Set.Make(E)

  type t = VSet.t * ESet.t

  let vertices g = fst g
  let edges g = snd g

  let mem_v g v = 
  try 
    VSet.find v (vertices g); true
  with
    Not_found -> false 

  let empty = (VSet.empty, ESet.empty)
  let add_e g e = let vertices' = VSet.add (E.src e) (vertices g) in
    (VSet.add (E.dst e) (vertices'), ESet.add e (edges g))
  let add_v g v = (VSet.add v (vertices g), edges g)

end
