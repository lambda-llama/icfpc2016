open Num
open Core_kernel.Std

open Types

let n = num_of_int

let search dst =
  let open Figure in
  let rec go iter = function
  | [] -> failwith "Engine.search: problem too hard"
  | acc ->
    let candidates = List.concat_map acc ~f:(fun f ->
        List.filter_map (segments f) ~f:(unfold f))
    in begin
      printf "iter %04d: %d candidates\n" iter (List.length candidates);
      match List.find candidates
              ~f:(fun f -> area f =/ n 1 && Figure.is_square_approx f) with
      | Some solution -> solution
      | None -> go (succ iter)
                  (List.filter candidates ~f:(fun f -> area f </ n 1))
    end
  in go 0 [dst]


let () as _test_search1 =
  let a = (n 0, n 0)
  and b = (n 0, div_num (n 1) (n 2))
  and c = (n 1, div_num (n 1) (n 2))
  and d = (n 1, n 0) in

  let skeleton = [(a, b); (b, c); (c, d); (d, a)] in
  let dst = Figure.of_skeleton skeleton in begin
    print_endline "== 1/2 square ==";
    print_endline (Output.output_figure (search dst) dst);
    flush stdout
  end


let () as _test_search2 =
  let h = div_num (n 1) (n 2) in
  let a = (n 0, n 0)
  and b = (n 0, h)
  and c = (h, h)
  and d = (n 1, n 0) in

  let skeleton = [(a, b); (b, c); (c, d); (d, a); (a, c)] in
  let dst = Figure.of_skeleton skeleton in begin
    print_endline "== spec. example ==";
    print_endline (Output.output_figure (search dst) dst);
    flush stdout
  end
