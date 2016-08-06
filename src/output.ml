open Num
open Core_kernel.Std

open Types

let s = string_of_num

let show_facet segments =
  segments
  |> List.map ~f:(fun ((x1, y1), (x2, y2)) ->
      sprintf "%s,%s,%s,%s" (s x1) (s y1) (s x2) (s y2))
  |> String.concat ~sep:"\n"


let skeleton_to_facets (s: string): string =
  String.split_lines s
  |> Input.skeleton_of_lines
  |> Figure.of_skeleton
  |> List.map ~f:show_facet
  |> String.concat ~sep:"\n\n"


module Vertextbl = Hashtbl.Make(Vertex.Key)

let facet_to_lines vs_map (f : Facet.t) =
  let vs = Facet.vertices f in
  let n_vertices = List.length vs in
  [Int.to_string n_vertices;
   List.map vs ~f:(Fn.compose Int.to_string (Vertextbl.find_exn vs_map))
   |> String.concat ~sep:" "]

let output_figure src dst =
  let vs_map = Vertextbl.create () in
  let () = List.iter (Figure.vertices src)
      ~f:(fun v -> Vertextbl.add_exn vs_map
             ~key:v ~data:(Vertextbl.length vs_map))
  in

  (* The source positions. *)
  let n_vertices = Vertextbl.length vs_map in
  let lines1 =
    (Int.to_string n_vertices)::
    (Hashtbl.to_alist vs_map
     |> List.map ~f:(fun (v, i) -> (i, Vertex.to_string v))
     |> List.sort ~cmp:(Tuple2.compare ~cmp1:Int.compare ~cmp2:String.compare)
     |> List.map ~f:snd)
  in

  (* The facets. *)
  let n_facets = List.length src in
  let lines2 = List.concat
      ([Int.to_string n_facets]::List.map src ~f:(facet_to_lines vs_map))
  in

  (* The destination positions. *)
  let lines3 =
    List.map (Figure.vertices dst) ~f:(fun v ->
        let i_opt = Vertextbl.find vs_map v in
        (Option.value ~default:(Vertextbl.length vs_map) i_opt,
         Vertex.to_string v))
    |> List.sort ~cmp:(Tuple2.compare ~cmp1:Int.compare ~cmp2:String.compare)
    |> List.map ~f:snd
  in String.concat ~sep:"\n" (List.concat [lines1; lines2; lines3])
