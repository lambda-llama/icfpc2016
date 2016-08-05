open Num
open Core_kernel.Std

let n = num_of_int

type coord = num [@printer fun fmt n -> fprintf fmt "%s" (string_of_num n)]
[@@deriving show]


module Vertex = struct
  type t = coord * coord [@@deriving show]

  let eq (x1, y1) (x2, y2) = eq_num x1 x2 && eq_num y1 y2

  let compare (x1, y1) (x2, y2) =
    match compare_num x1 x2 with
    | 0      -> compare_num y1 y2
    | result -> result

  let sub (x1, y1) (x2, y2) = (x1 -/ x2, y1 -/ y2)

  let dot (x1, y1) (x2, y2) = x1 */ x2 +/ y1 */ y2

  let norm v = dot v v

  (** Reflects a coordinate agains a line segment [a, b]. *)
  let reflect (x, y) (a, b) =
    let (dx, dy) as d = sub b a in
    let u = div_num (dx */ dx -/ dy */ dy) (norm d)
    and v = div_num (n 2 */ dx */ dy) (norm d) in

    (* http://math.stackexchange.com/q/65503/113653 *)
    let (ax, ay) = a in
    (u */ (x -/ ax) +/ v */ (y -/ ay) +/ ax,
     v */ (x -/ ax) -/ u */ (y -/ ay) +/ ay)

  let () as _test_reflect = begin
    assert (reflect (n 0, n 0) ((n 1, n 0), (n 1, n 1)) = (n 2, n 0));
    assert (reflect (n 2, n 0) ((n 1, n 0), (n 1, n 1)) = (n 0, n 0))
  end
end


module Segment = struct
  type t = Vertex.t * Vertex.t [@@deriving show]

  let eq (a, b) (c, d) = Vertex.eq a c && Vertex.eq b d
  let eq_unordered (a, b) (c, d) =
    (Vertex.eq a c && Vertex.eq b d) ||
    (Vertex.eq a d && Vertex.eq b c)

  let compare (a, b) (c, d) =
    match Vertex.compare a c with
    | 0      -> Vertex.compare b d
    | result -> result
end

type poly = Vertex.t list

and silhouette = poly list

and skeleton = Segment.t list

and problem = silhouette * skeleton
[@@deriving show]

(** Sort vertices in counter-clockwise order. *)
let sort_cc =
  let angle (x, y) = atan (float_of_num (div_num y x)) in
  List.sort ~cmp:(fun v1 v2 -> compare (angle v1) (angle v2))


module Facet = struct
  type t = Segment.t list [@@deriving show]

  (* XXX possibly duplicated. *)
  let _vertices f = List.concat_map f ~f:(fun (a, b) -> [a; b])

  let intersects (f: t) (other: t) =
    List.exists f ~f:(List.mem ~equal:Segment.eq_unordered other)

  (* http://mathworld.wolfram.com/PolygonArea.html *)
  let area f =
    let s = List.fold f ~init:(num_of_int 0)
        ~f:(fun acc ((x1, y1), (x2, y2)) -> acc +/ (x1 */ y2 -/ x2 */ y1))
    in div_num s (num_of_int 2)

  let reflect f s = List.rev_map f ~f:(fun (a, b) ->
      (Vertex.reflect b s, Vertex.reflect a s))

  let () as _test_reflect =
    let a = (n 0, n 0)
    and b = (n 0, n 1)
    and c = (n 1, n 1)
    and d = (n 1, n 0)
    in begin
      assert (reflect [(a, b); (b, c); (c, d); (d, a)] (c, d) = [
          ((n 2, n 0), d); (d, c); (c, (n 2, n 1)); ((n 2, n 1), (n 2, n 0))
        ])
    end

  let next_cc_segment in_seg out_segs =
    let angle (s1, e1) (s2, e2) =
      assert (Vertex.eq e1 s2);
      let (x1, y1) = Vertex.sub s1 e1 in (* opposite direction *)
      let (x2, y2) = Vertex.sub e2 s2 in
      let f = float_of_num in
      let clap x = if x < 0.0 then x +. 2.0 *. 3.141592 else x in
      let a x y = clap (atan2 (f y) (f x)) in
      clap ((a x2 y2) -. (a x1 y1))
    in
    Option.value_exn (List.max_elt out_segs ~cmp:(fun x y -> if angle in_seg x < angle in_seg y then -1 else 1))

  module SegmentMap = Hashtbl.Make(struct
      type t = Vertex.t

      let sexp_of_t _ = failwith "not implemented"
      and t_of_sexp _ = failwith "not implemented"

      let compare = Vertex.compare
      and hash = Hashtbl.hash
    end)

  let of_skeleton (s: skeleton) : t list =
    let half_edges = ref (List.concat_map s ~f:(fun (a, b) -> [(a, b); (b, a)])) in
    let segmap = SegmentMap.create () in
    let poly_of_segment (start: Segment.t) : Segment.t list =
      let next s =
        SegmentMap.find_exn segmap (snd s)
        |> List.filter ~f:(fun (a, b) -> not (Segment.eq (b, a) s))
        |> next_cc_segment s
      in let rec go work =
           let n = next (List.hd_exn work) in
           assert (n <> List.hd_exn work);
           if Segment.eq n start then work else go (n :: work)

      in go [start]
    in
    let result = ref [] in
    begin
      List.iter s ~f:(fun (a, b) -> begin
            SegmentMap.add_multi segmap ~key:a ~data:(a, b);
            SegmentMap.add_multi segmap ~key:b ~data:(b, a);
            ()
          end);
      while not (List.is_empty !half_edges) do
        let next = List.hd_exn !half_edges in
        let facet = poly_of_segment next in
        begin
          half_edges := List.filter !half_edges
              ~f:(fun e -> List.for_all facet ~f:(fun ee -> not (Segment.eq e ee)));
          result := facet :: !result
        end
      done;
      !result
    end

  let () =
    let n = num_of_int in
    let up = ((n 0, n 0), (n 0, n 1)) in
    let down = ((n 0, n 2), (n 0, n 1)) in
    let left = ((n 0, n 1), (n ~-1, n 1)) in
    let right = ((n 0, n 1), (n  1, n 1)) in
    let ans1 = next_cc_segment up [left; right] in
    let ans2 = next_cc_segment down [left; right] in
    assert (ans1 = left);
    assert (ans2 = right)

  let () =
    let n = num_of_int in
    let up = ((n 1, n 0), (n 1, n 1)) in
    let right = ((n 1, n 1), (n  0, n 1)) in
    let down_right = ((n 1, n 1), (n  0, n 0)) in
    let ans = next_cc_segment up [right; down_right] in
    assert (ans = down_right)

  let () =
    let n = num_of_int in
    let h = div_num (n 1) (n 2) in
    let a = (n 0, n 0) in
    let b = (n 1, n 0) in
    let c = (h, h) in
    let d = (n 0, h) in
    let skel = [
        (a, b);
        (b, c);
        (c, d);
        (a, d);
        (a, c);
      ] in
    let facets = of_skeleton skel in
    ()
end


module Figure = struct
  type t = Facet.t list [@@deriving show]

  let vertices f =
    let result = List.concat_map f
        ~f:(List.concat_map ~f:(fun (a, b) -> [a; b]))
    in result |> List.dedup |> sort_cc

  let segments f = List.concat f |> List.dedup

  (** Unfolds a given segment [s] of a figure [f]. *)
  let unfold f s =
    List.find f ~f:(fun target -> List.mem ~equal:Segment.eq target s)
    |> Option.map ~f:(fun target ->
        let neigbours = List.filter f ~f:(fun other ->
            not (phys_equal other target) &&
            Facet.intersects other target)
        in

        (* print_newline (); *)
        (* printf ">>> target = %s\n" (Facet.show target); *)
        (* List.iter neigbours ~f:(fun n -> printf ">>> neigbour = %s\n" (Facet.show n)); *)

        (* Remove segements common with [target] from neighbours. *)
        let skeleton = List.map neigbours
            ~f:(List.filter ~f:(fun s ->
                not @@ List.mem ~equal:Segment.eq_unordered target s))
        in

        (* List.iter skeleton ~f:(fun n -> *)
        (*     printf "\n-----\n"; *)
        (*     List.iter n ~f:(fun x -> printf ">>> segment = %s\n" (Segment.show x))); *)

        (* Remove the segment used for reflection from the result. *)
        let reflected =
          Facet.reflect target s |> List.filter ~f:(Segment.eq s)
        in

        let replacement = List.concat_no_order (reflected::skeleton) in
        let remaining = List.filter f ~f:(fun other ->
            List.mem ~equal:phys_equal neigbours other ||
            phys_equal other target)
        in replacement::remaining)  (* TODO: SORT! *)

  let () as _unfold_test =
    let a = (n 0, n 0)
    and b = (n 0, n 1)
    and c = (n 1, n 1)
    and d = (n 1, n 0) in

    let upper_triangle: Facet.t = [(a, b); (b, c); (c, a)]
    and lower_triangle: Facet.t = [(a, c); (c, d); (d, a)] in

    let f: t = [upper_triangle; lower_triangle] in
    ()

  let area = List.fold_left ~init:(num_of_int 0)
      ~f:(fun acc f -> acc +/ Facet.area f)

  let is_square f =
    let is_orthogonal (x1, y1) (x2, y2) =
      (x2 -/ x1) +/ (y2 -/ y1) = num_of_int 0
    in match vertices f with
    | [a; b; c; d] -> is_orthogonal (Vertex.sub b a) (Vertex.sub c b) &&
                      is_orthogonal (Vertex.sub c b) (Vertex.sub d c)
    | _other       -> false
end
