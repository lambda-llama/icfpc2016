open Num
open Core_kernel.Std


let n = num_of_int

let at_origin w h =
  let zero = n 0
  and one = n 1 in
  let points_before = [
    (zero, zero); (one, zero); (one, one); (zero, one);
    (w, zero); (one, h); (w, one); (zero, h);
    (w, h)
  ] in
  let points_after =
    let one_wr = w -/ (one -/ w) in
    let one_hr = h -/ (one -/ h) in [
    (zero, zero); (one_wr, zero); (one_wr, one_hr); (zero, one_hr);
    (w, zero); (one_wr, h); (w, one_hr); (zero, h);
    (w, h)
    ] in
  let facets = [
    [0; 4; 8; 7];
    [4; 8; 5; 1];
    [8; 5; 2; 6];
    [6; 8; 7; 3];
  ]

  in (points_before, facets, points_after)

let mk_rot cos sin =
  fun (x, y) -> (x */ cos -/ y */ sin, x */ sin +/ y */ cos)


let solve_inner skeleton =
  let vs = List.concat_map skeleton ~f:(fun (a, b) -> [a; b]) in
  let (x_min, x_max) = Internal.min_max
      ~compare:compare_num (List.map vs ~f:fst)
  and (y_min, y_max) = Internal.min_max
      ~compare:compare_num (List.map vs ~f:snd)
  in
  let w = x_max -/ x_min
  and h = y_max -/ y_min
  in
  if w <=/ n 1 && h <=/ n 1
  then
    let f (x, y) = (x +/ x_min, y +/ y_min) in
    let (before, facets, after) = at_origin w h in
    Some (before, facets, List.map after ~f)
  else None


let string_of_solution before facets after =
  sprintf "%d\n%s\n%d\n%s\n%s"
    (List.length before)
    (List.map before ~f:Vertex.to_string |> String.concat ~sep:"\n")
    (List.length facets)
    (List.map ~f:(fun f ->
         sprintf "%d %s"
           (List.length f)
           (List.map f ~f:Int.to_string |> String.concat ~sep:" ")
       )
       facets |> String.concat ~sep:"\n")
    (List.map after ~f:Vertex.to_string |> String.concat ~sep:"\n")


let bf_solve skel =
  match solve_inner skel with
  | Some (before, facets, after) ->
    Some (string_of_solution before facets after)
  | None -> None
