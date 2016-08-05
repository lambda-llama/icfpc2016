open Num
open Core_kernel.Std

module Coord = struct
  type t = num [@printer fun fmt n -> fprintf fmt "%s" (string_of_num n)]
  [@@deriving show]

  let sub (x1, y1) (x2, y2) = (x1 -/ x2, y1 -/ y2)
end

type vertex = Coord.t * Coord.t

and segment = vertex * vertex

and poly = vertex list

and silhouette = poly list

and skeleton = segment list

and problem = silhouette * skeleton
[@@deriving show]


(** Sort vertices in counter-clockwise order. *)
let sort_cc =
  let angle (x, y) = atan (float_of_num (div_num y x)) in
  List.sort ~cmp:(fun v1 v2 -> compare (angle v1) (angle v2))


module Facet = struct
  type t = segment list

  (* XX possibly duplicated. *)
  let _segments = Fn.id

  (* XXX possibly duplicated. *)
  let _vertices f = List.concat_map f ~f:(fun (a, b) -> [a; b])

  let _mem = List.mem

  let intersects f other = List.for_all f ~f:(List.mem other)

  (* http://mathworld.wolfram.com/PolygonArea.html *)
  let area f =
    let s = List.fold f ~init:(num_of_int 0)
        ~f:(fun acc ((x1, y1), (x2, y2)) -> acc +/ (x1 */ y2 -/ x2 */ y1))
    in div_num s (num_of_int 2)
end


module Figure = struct
  type t = Facet.t list

  let vertices f =
    List.concat_map f ~f:Facet._vertices |> List.dedup |> sort_cc

  let segments f = List.concat_map f ~f:Facet._segments |> List.dedup

  (** Unfolds a given segment [s] of a figure [f]. *)
  let unfold f s =
    List.find f ~f:(fun target -> Facet._mem target s)
    |> Option.map ~f:(fun target ->
        let neigbours = List.filter f ~f:(Facet.intersects target) in
        failwith "not implemented"
      )

  let area = List.fold_left ~init:(num_of_int 0)
      ~f:(fun acc f -> acc +/ Facet.area f)

  let is_square f =
    let is_orthogonal (x1, y1) (x2, y2) =
      (x2 -/ x1) +/ (y2 -/ y1) = num_of_int 0
    in match vertices f with
    | [a; b; c; d] -> is_orthogonal (Coord.sub b a) (Coord.sub c b) &&
                      is_orthogonal (Coord.sub c b) (Coord.sub d c)
    | _other       -> false
end
