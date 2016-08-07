open Core_kernel.Std

open Internal

let show_facet {Facet.segments=ss} =
  List.map ss ~f:(fun ((x1, y1), (x2, y2)) ->
      sprintf "%s,%s,%s,%s" (s x1) (s y1) (s x2) (s y2))
  |> String.concat ~sep:"\n"


let skeleton_to_facets (s: string): string =
  String.split_lines s
  |> Input.skeleton_of_lines
  |> Figure.of_skeleton
  |> List.map ~f:show_facet
  |> String.concat ~sep:"\n\n"


module VT = Hashtbl.Make(Vertex)


let facet_to_lines vs_map (f : Facet.t) =
  let vs = Facet.vertices f in
  let n_vertices = List.length vs in
  [Int.to_string n_vertices;
   List.map vs ~f:(Fn.compose Int.to_string (VT.find_exn vs_map))
   |> String.concat ~sep:" "]


let output_solution src_dst_map src dst =
  let vs_map = VT.create () in
  let () = List.iter (VT.keys src_dst_map)
      ~f:(fun v -> VT.add_exn vs_map
             ~key:v ~data:(VT.length vs_map))
  in

  (* The source positions. *)
  let n_vertices = VT.length vs_map in
  let src_vs =
    VT.to_alist vs_map
    |> List.map ~f:(fun (v, i) -> (i, v))
    |> List.sort ~cmp:(Tuple2.compare ~cmp1:Int.compare ~cmp2:Vertex.compare)
    |> List.map ~f:snd
  in

  let lines1 = Int.to_string n_vertices::List.map src_vs ~f:Vertex.to_string in

  (* The facets. *)
  let n_facets = List.length src in
  let lines2 = List.concat
      ([Int.to_string n_facets]::List.map src ~f:(facet_to_lines vs_map))
  in

  (* The destination positions. *)
  let dst_vs = List.map src_vs ~f:(VT.find_exn src_dst_map) in
  let lines3 = List.map dst_vs ~f:Vertex.to_string in
  String.concat ~sep:"\n" (List.concat [lines1; lines2; lines3])
