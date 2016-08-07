open Num
open Core_kernel.Std

open Internal

type t = {segments: Segment.t list; area: coord}
[@@deriving show]

module Vertex2 = struct
  type nonrec t = Vertex.t * Vertex.t

  let sexp_of_t _ = stub "Vertex2.sexp_of_t"
  and t_of_sexp _ = stub "Vertex2.t_of_sexp"

  let compare = Tuple2.compare ~cmp1:Vertex.compare ~cmp2:Vertex.compare
  and hash = Hashtbl.hash
end

let _segment_area =
  let go ((x1, y1), (x2, y2)) = (x1 */ y2 -/ x2 */ y1) in
  let hashable = Hashtbl.Hashable.of_key(module Vertex2) in
  Memo.general ~hashable go

(** Checks that all facet segments are consecutive. *)
let _is_proper = function
| [] | [_] -> true
| ((first, _)::_)  as ss ->
  let rec go acc = function
  | [] -> failwith "Facet.is_proper: impossible"
  | [(_, last)] -> (acc, last)
  | (_a, b)::(((c, _d)::_) as rest) ->
    go (acc && Vertex.eq b c) rest
  in

  let (acc, last) = go true ss in acc && Vertex.eq first last

let create ss =
  assert (_is_proper ss);
  (* http://mathworld.wolfram.com/PolygonArea.html *)
  let s = List.fold_left ss ~init:(n 0)
      ~f:(fun acc ab -> acc +/ _segment_area ab)
  in {segments=ss; area=div_num s (num_of_int 2)}

let area {area; _} = area

let compare {segments=ss1; area=a1} {segments=ss2; area=a2} =
  match compare_num a1 a2 with
  | 0      -> List.compare Segment.compare ss1 ss2
  | result -> result

let eq {segments=ss1; area=a1} {segments=ss2; area=a2} =
  eq_num a1 a2 && List.equal ~equal:Segment.eq ss1 ss2

let rec fix (ss : Segment.t list): Segment.t list =
  let merge_segments (a, b) (b', c) =
    assert (Vertex.eq b b');
    let open Vertex in
    let v1 = sub b a in
    let v2 = sub b' c in
    let d = dot v1 v2 in
    if d */ d =/ dot v1 v1 */ dot v2 v2 then Some (a, c) else None
  in match ss with
  | []  -> []
  | [x] -> [x]
  | x::y::rest -> begin match merge_segments x y with
    | Some z -> z::fix rest
    | None   -> x::fix (y::rest)
    end

let merge ({segments=ss1; _} as f1) ({segments=ss2; _} as f2): t =
  let split_on_edge f on =
    let (l, r) = List.split_while f ~f:(Segment.neq on) in
    (l, List.tl_exn r)
  in

  let common = List.filter ss1
      ~f:(fun s -> List.mem ~equal:Segment.eq ss2 (Segment.twin s))
  in match common with
  | [on] ->
    let (l1, r1) = split_on_edge ss1 on
    and (l2, r2) = split_on_edge ss2 (Segment.twin on) in
    create @@ fix (List.concat [l1; r2; l2; r1])
  | _other ->
    let debug = sprintf "\n%s\n%s" (show f1) (show f2) in
    if common = []
    then failwith ("Facet.merge: no common segments" ^ debug)
    else failwith ("Facet.merge: >1 common segments" ^ debug)

let intersects {segments=ss1; _} {segments=ss2; _} = List.exists ss1
    ~f:(fun s -> List.mem ~equal:Segment.eq ss2 (Segment.twin s))

let vertices {segments=ss; _} = List.map ~f:fst ss
and segments {segments=ss; _} = ss

let map {segments=ss; _} ~f = create @@ List.map ~f ss

let reflect {segments=ss; area} s =
  let reflected = List.rev_map ss ~f:(fun (a, b) ->
      (Vertex.reflect b s, Vertex.reflect a s))
  in {segments=reflected; area}

and quasi_reflect {segments=ss; area} =
  let reflected = List.rev_map ss ~f:Segment.twin in
  {segments=reflected; area}
