open Num
open Core_kernel.Std

open Types

let n = num_of_int

let map_src_dst src dst =
  let src_dst_map = Vertextbl.create () in
  let ps = List.zip_exn (Figure.vertices src) (Figure.vertices dst) in
  let result = List.for_all ps ~f:(fun (a, b) ->
      match Vertextbl.find src_dst_map a with
      | Some c -> Vertex.eq b c
      | None   -> Vertextbl.add_exn src_dst_map ~key:a ~data:b; true)
  in if result then Some src_dst_map else None


let search dst: string =
  let open Figure in
  let rec go iter = function
  | [] -> failwith "Engine.search: problem too hard"
  | candidates ->
    let solutions =
      List.filter candidates ~f:(fun (dst, src) -> area src =/ n 1)
      |> List.filter_map
        ~f:(fun (dst, src) ->
            Option.bind (to_unit_square src) (fun tus ->
                let unit_src = Figure.transform_figure tus src in
                Option.map (map_src_dst unit_src dst) ~f:(fun m ->
                    Output.output_solution m unit_src dst)))
    in match solutions with
    | (solution::rest) ->
      fprintf stderr "iter %04d: %d extra solutions found\n" iter (List.length rest);
      solution
    | _other ->
      List.concat_map candidates ~f:(fun (dst, src) ->
          List.concat_map (segments src) ~f:(fun s ->
              List.filter (unfold dst src s)
                ~f:(fun (_dst, src) -> area src <=/ n 1)))
      |> go (succ iter)
  in go 1 [(dst, dst)]
