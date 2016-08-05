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

  open Num

  let vsub (x1, y1) (x2, y2) = ((x1 -/ x2), (y1 -/ y2))
  let dot (x1, y1) (x2, y2) = x1 */ x2 +/ y1 */ y2

  let of_skeleton (s: skeleton) : t list =
    let half_edges = ref (List.concat_map s ~f:(fun (a, b) -> [(a, b); (b, a)])) in
    let segmap = Hashtbl.Poly.create () in
    let angle (s1, e1) (s2, e2) =
      assert (e1 = s2);
      let d1 = vsub e1 s1 in
      let d2 = vsub e2 s2 in
      let l1 = dot d1 d1 |> Num.float_of_num |> sqrt in
      let l2 = dot d2 d2 |> Num.float_of_num |> sqrt in
      let cos = (Num.float_of_num (dot d1 d2)) /. l1 /.l2 in
      cos
    in
    let poly_of_segment (start: segment) : segment list =
      let next s =
        Option.value_exn
        (Hashtbl.Poly.find_exn segmap (snd s)
         |> List.min_elt ~cmp:(fun x y -> if angle s x < angle s y then (-1) else 1))
      in let rec go work =
           let n = next (List.hd_exn work) in
           if n = start then work else go (n :: work)

      in go [start]
    in
    let result = ref [] in
    begin
      List.iter s ~f:(fun (a, b) -> begin
            Hashtbl.add_multi segmap ~key:a ~data:(a, b);
            Hashtbl.add_multi segmap ~key:b ~data:(b, a);
            ()
          end);
      while not (List.is_empty !half_edges) do
        let next = List.hd_exn !half_edges in
        let facet = poly_of_segment next in
        begin
          half_edges := List.filter !half_edges ~f:(fun e -> List.for_all facet ~f:(fun ee -> e <> ee));
          result := facet :: !result
        end
      done;
      !result
    end
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
