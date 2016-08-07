open Num
open Core_kernel.Std

open Internal

type t = Facet.t list [@@deriving show]

let compare: t -> t -> int = List.compare Facet.compare

let dumps_segments ?l:(l="") segs =
  let s = List.map segs ~f:Segment.show
  |> String.concat ~sep:", " in
  prerr_string (l ^ "[" ^ s ^ "]");
  prerr_newline(); prerr_newline()

let next_cc_segment in_seg out_segs =
  assert (not @@ List.is_empty out_segs);
  let angle (s1, e1) (s2, e2) =
    assert (Vertex.eq e1 s2);
    let (x1, y1) = Vertex.sub s1 e1 in (* opposite direction *)
    let (x2, y2) = Vertex.sub e2 s2 in
    let f = float_of_num in
    let clap x = if x < 0.0 then x +. 2.0 *. 3.141592 else x in
    let a x y = clap (atan2 (f y) (f x)) in
    clap ((a x2 y2) -. (a x1 y1))
  in
  Option.value_exn (List.max_elt out_segs ~cmp:(fun x y ->
      Pervasives.compare (angle in_seg x) (angle in_seg y)))

let rec inner_intersections segs =
  let aux x xs =
    let f (c, d) =
      let (a, b) = x in
      let v = Vertex.sub b a in
      let (ux, uy) = Vertex.sub d c in
      let o = (uy, n 0 -/ ux) in
      (* a + t v = c + ? u *)
      assert (Vertex.dot o (ux, uy) =/ n 0);
      let l = Vertex.dot o v in
      let k = Vertex.dot o (Vertex.sub c a) in
      if l =/ n 0
      then None
      else Some(Vertex.add a (Vertex.scale (Num.div_num k l) v))
    in
    List.filter_map xs ~f
  in
  match segs with
  | [] -> []
  | x::xs -> aux x xs @ inner_intersections xs

let split_segments segs =
  let is_between b e mid =
    if Vertex.eq b mid || Vertex.eq e mid
    then false
    else
      let v1 = Vertex.sub b mid in
      let v2 = Vertex.sub e mid in
      let d = (Vertex.dot v1 v2) in
      d </ n 0 && d */ d =/ Vertex.dot v1 v1 */ Vertex.dot v2 v2 in
  let vertecies = List.dedup ~compare:Vertex.compare
      (inner_intersections segs @ List.concat_map segs ~f:(fun (a, b) -> [a; b])) in
  let rec go segs =
    match segs with
    | [] -> []
    | (b, e)::xs -> match List.find vertecies ~f:(is_between b e) with
      | None -> (b, e)::go xs
      | Some mid -> go ((b, mid)::(mid, e)::xs)
  in
  go segs

module VT = Hashtbl.Make(Vertex)

let of_skeleton (skel: Segment.t list): t =
  printf "facetting [%s]\n" (String.concat ~sep:"," @@ List.map skel ~f:Segment.show);
  let s = split_segments skel in
  let half_edges = ref (List.concat_map s ~f:(fun (a, b) -> [(a, b); (b, a)])) in
  let segmap = VT.create () in
  List.iter !half_edges ~f:(fun (a, b) ->
      VT.add_multi segmap ~key:a ~data:(a, b));

  let poly_of_segment (start: Segment.t) : Segment.t list =
    let next s =
      VT.find_exn segmap (snd s)
      |> List.filter ~f:(fun (a, b) -> Segment.neq (b, a) s)
      |> next_cc_segment s
    in let rec go work =
         let n = next (List.hd_exn work) in
         assert (Segment.neq n (List.hd_exn work));
         if Segment.eq n start then work else go (n :: work)
    in go [start]
  in

  let result = ref [] in begin
    while not (List.is_empty !half_edges) do
      let next = List.hd_exn !half_edges in
      let ss = List.rev @@ poly_of_segment next in
      begin
        half_edges := List.filter !half_edges
            ~f:(fun e -> not (List.mem ~equal:Segment.eq ss e));

        let f = Facet.create ss in
        if Facet.area f >/ n 0 then
          result := f::!result
      end
    done; !result
  end

let vertices f = List.concat_map f ~f:Facet.vertices

let segments f =
  List.concat_map f ~f:Facet.segments |> List.dedup ~compare:Segment.compare

let area = List.fold_left ~init:(n 0)
    ~f:(fun acc f -> acc +/ Facet.area f)

let unfold (dst : t) (src : t) s =
  let mapper = List.zip_exn src dst in
  let targets = List.filter src ~f:(fun target ->
      List.mem ~equal:Segment.eq_unordered (Facet.segments target) s)
  in match targets with
  | [target] ->
     let neighbours = List.filter src ~f:(fun other ->
        not (phys_equal other target) &&
        Facet.intersects other target)
     in

     let step (dst, src) target =
       let unfolded = Facet.reflect target s::src in
       if area unfolded >/ n 1
       then None
       else
         let dtarget = List.Assoc.find_exn ~equal:Facet.eq mapper target
         in Some (Facet.quasi_reflect dtarget::dst, unfolded)
     in begin match step (dst, src) target with
     | Some base -> base::List.filter_map neighbours ~f:(step base)
     | None      -> []
     end
  | _other -> []  (* 0 or >1 *)

let transform_facet m = Facet.map ~f:(fun (a, b) -> (m a, m b))
let transform_figure m t = List.map t ~f:(transform_facet m)

let is_square_approx (f : t) =
  let vs = vertices f in
  let (x_min, x_max) = Internal.min_max
      ~compare:compare_num (List.map vs ~f:fst)
  and (y_min, y_max) = Internal.min_max
      ~compare:compare_num (List.map vs ~f:snd)
  in x_max -/ x_min =/ n 1 && y_max -/ y_min =/ n 1

let mk_rot cos sin =
  fun (x, y) -> (x */ cos -/ y */ sin, x */ sin +/ y */ cos)

let to_unit_square (f: t) : (Vertex.t -> Vertex.t) option =
  let vs = vertices f in
  let (x_min, x_max) = Internal.min_max
      ~compare:compare_num (List.map vs ~f:fst)
  and (y_min, y_max) = Internal.min_max
      ~compare:compare_num (List.map vs ~f:snd)
  in if x_max -/ x_min =/ n 1 && y_max -/ y_min =/ n 1
  then Some (fun (x, y) -> (x -/ x_min, y -/ y_min))
  else
    let ((top: Vertex.t), left, bottom, right) =
      let by_x a b = compare_num (fst a) (fst b)
      and by_y a b = compare_num (snd a) (snd b) in
          (Option.value_exn (List.max_elt vs ~cmp:by_y),
           Option.value_exn (List.min_elt vs ~cmp:by_x),
           Option.value_exn (List.min_elt vs ~cmp:by_y),
           Option.value_exn (List.max_elt vs ~cmp:by_x)) in
    let open Vertex in
    let is_unit p1 p2 =
      let d = sub p1 p2 in dot d d =/ n 1
    in if is_unit top left && is_unit left bottom &&
          is_unit bottom right && is_unit right top
    then
      let translate = fun (x, y) -> (x -/ fst left, y -/ snd left) in
      let rotate =
        let (cos, nsin) = translate bottom in
        assert (cos */ cos +/ nsin */ nsin =/ n 1);
        let sin = n 0 -/ nsin in
        mk_rot cos sin
      in Some (Fn.compose rotate translate)
    else None
