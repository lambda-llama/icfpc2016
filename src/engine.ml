open Num
open Core_kernel.Std

open Internal

module VT = Hashtbl.Make(Vertex)

let map_src_dst src dst =
  let src_dst_map = VT.create () in
  let ps = List.zip_exn (Figure.vertices src) (Figure.vertices dst) in
  let result = List.for_all ps ~f:(fun (a, b) ->
      match VT.find src_dst_map a with
      | Some c -> Vertex.eq b c
      | None   -> VT.add_exn src_dst_map ~key:a ~data:b; true)
  in if result then Some src_dst_map else None


let compare_src (_dst1, src1) (_dst2, src2) = Figure.compare src1 src2
[@@inline]

let search dst: string =
  let open Figure in
  let rec go iter = function
  | [] -> failwith "Engine.search: problem too hard"
  | candidates ->
    printf "iter %04d: %d candidates" iter (List.length candidates);
    print_newline ();
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
      printf "iter %04d: %d extra solutions found\n" iter (List.length rest);
      solution
    | _other ->
      let new_candidates = List.concat_map candidates ~f:(fun (dst, src) ->
          List.concat_map (segments src) ~f:(fun s ->
              List.filter (unfold dst src s)
                ~f:(fun (_dst, src) -> area src <=/ n 1)))
      in begin
        go (succ iter) (List.dedup ~compare:compare_src new_candidates)
      end
  in go 1 [(dst, dst)]
