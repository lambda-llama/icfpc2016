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
  in if result then Some src_dst_map else ((if Figure._is_target src then
                                              List.iter ps ~f:(fun (a, b) -> printf "%s --> %s\n" (Vertex.to_string a) (Vertex.to_string b))); printf "\n\n"; None)


let search dst: string =
  let open Figure in
  let rec go iter = function
  | [] -> failwith "Engine.search: problem too hard"
  | acc ->
    let candidates = List.concat_map acc ~f:(fun (dst, src) ->
        List.concat_map (segments src) ~f:(fun s ->
            List.filter (unfold dst src s)
              ~f:(fun (_dst, src) -> area src <=/ n 1)))
    in begin
      printf "iter %04d: %d candidates\n" iter (List.length candidates);
      let solutions =
        List.filter candidates
          ~f:(fun (dst, src) -> area src =/ n 1 && is_square_approx src)
        |> List.filter_map
          ~f:(fun (dst, src) -> Option.map (map_src_dst src dst)
                 (fun m -> Output.output_solution m src dst))
      in match solutions with
      | (solution::rest) ->
        printf "iter %04d: %d extra solutions found\n" iter (List.length rest);
        solution
      | _other -> go (succ iter) candidates
    end
  in go 0 [(dst, dst)]


(* let () as _test_search1 = *)
(*   let a = (n 0, n 0) *)
(*   and b = (n 0, div_num (n 1) (n 2)) *)
(*   and c = (n 1, div_num (n 1) (n 2)) *)
(*   and d = (n 1, n 0) in *)

(*   let skeleton = [(a, b); (b, c); (c, d); (d, a)] in *)
(*   let dst = Figure.of_skeleton skeleton in begin *)
(*     print_endline "== 1/2 square =="; *)
(*     print_endline @@ search dst; *)
(*     flush stdout *)
(*   end *)


let () as _test_search2 =
  let h = div_num (n 1) (n 2) in
  let a = (n 0, n 0)
  and b = (n 0, h)
  and c = (h, h)
  and d = (n 1, n 0) in

  let skeleton = [(a, b); (b, c); (c, d); (d, a); (a, c)] in
  let dst = Figure.of_skeleton skeleton in begin
    let h = div_num (n 1) (n 2) in
    print_endline "== spec. example ==";
    print_endline @@ search dst;
    flush stdout
  end
