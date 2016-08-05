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
