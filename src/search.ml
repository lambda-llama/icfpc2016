open Core_kernel.Std

type frac = ()

type point = (frac * frac)

type edge = (point * point)

let reflect_edge (e: edge) (m: edge) : edge = ()

type face = edge list

type shade = face list

let has_edge (f: face) (e: edge) = List.exists f ~f:((=) e)

let reflect (f: face) (m: edge) : face =
  List.map f ~f:(fun e -> reflect_edge e m)
  |> List.rev

let face_area (x: face): frac =
  let open Num in
  List.map face ~f:(fun ((x1, y1), (x2, y2)) -> x1 * y2 - x2 * y1)
  |> List.sum

let split_on_edge (f: face) (e: edge) : (edge list * edge list) =
  let (l, r) = List.split_while f ~f:((=) e ) in
  (l, List.tl_exn r)

let merge (f1: face) (f2: face) (e: edge): face =
  let (l1, r1) = split_on_edge f1 e in
  let (l2, r2) = split_on_edge f2 e in
  l1 @ r2 @ l2 @ r1

let neightbor (s: shade) (f: face) (e: edge) : face option =
  let neighbors = List.filter s ~f:(fun f -> has_edge f e)
                  |> List.filter ~f:((<>) f)
  in match List.length neighbors with
  | 0 -> None
  | 1 -> Some (List.hd_exn neighbors)
  | _ -> failwith "More than one neighbor :("

let shade_area (s: shade): frac = failwith "TODO"

let unfold_edge (s: shade) (f: face) (e: edge) : shade option =
  let new_face = reflect f e in
  match neightbor s f e with
  | Some _ -> None
  | None -> Some (new_face :: s)

let unfold_face (s: shade) (f: face) : shade list =
  List.filter_map f ~f:(unfold_edge s f)

let unfold_all (s: shade) : shade list =
  List.concat_map s ~f:(unfold_face s)

let unfold_once (s: shade) : shade list =
  unfold_all s
  |> List.filter ~f:(fun s -> shade_area s < 1.0)

let rec find_candidates (s: shade) : shade list =
  (List.concat_map (unfold_once s)~f:find_candidates)
  @ if area s == 1 then [s] else []
