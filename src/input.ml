open Num
open Core_kernel.Std


let unfold n ~init ~f =
  let rec go acc state = function
  | 0 -> (acc, state)
  | n ->
    let (result, modified) = f state in
    go (result::acc) modified (pred n)
  in go [] init n


let vertex_of_string s =
  match String.split_on_chars ~on:[','] s with
  | [x; y] -> (num_of_string x, num_of_string y)
  | _other -> failwiths "invalid vertex" s sexp_of_string


let shade_of_lines lines =
  let n_poly = Int.of_string @@ List.hd_exn lines in
  unfold n_poly ~init:(List.tl_exn lines) ~f:(function
    | [] -> failwiths "invalid shade" lines (sexp_of_list sexp_of_string)
    | (line::rest) ->
      let n_vertices = Int.of_string line in
      let (chunk, rest) = List.split_n rest n_vertices in
      (List.map chunk ~f:vertex_of_string), rest)


let skeleton_of_lines lines =
  let _n_segments = Int.of_string @@ List.hd_exn lines in
  List.map (List.tl_exn lines) ~f:(fun line ->
      match String.split_on_chars ~on:[' '] line with
      | [source; target] -> (vertex_of_string source,
                             vertex_of_string target)
      | _other           -> failwiths "invalid segment" line sexp_of_string)


let problem_of_string s =
  let (shade, rest) = shade_of_lines @@ String.split_lines s in
  let skeleton = skeleton_of_lines rest in
  (shade, skeleton)
