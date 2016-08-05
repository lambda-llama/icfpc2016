type coord = Num.num
    [@printer fun fmt n -> fprintf fmt "%s" (Num.string_of_num n)]

and vertex = coord * coord

and segment = vertex * vertex

and poly = vertex list

and silhouette = poly list

and skeleton = segment list

and problem = silhouette * skeleton
[@@deriving show]


module Facet = struct
  type t
end


module Figure : sig
  type t = Facet.t list

  val edges : t -> edge list
  val unfold : t -> edge -> t option
  val area : t -> num
  val is_square : t -> bool
end = struct
end
