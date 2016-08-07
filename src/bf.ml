open Num
open Core_kernel.Std

open Internal

let zero = n 0
let one = n 1

let uber_stupid =
  (
    [(zero, zero); (one, zero); (one, one); (zero, one)],
    [[0; 1; 2; 3]],
    [(zero, zero); (one, zero); (one, one); (zero, one)]
  )

let at_origin w h =
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


let closest_angle ((x1, y2), (x2, y1)) =
  let f = float_of_num in
  let dx = (f x2) -. (f x1)
  and dy = (f y2) -. (f y1) in
  let nd = sqrt (dx *. dx +. dy *. dy) in
  let angle = acos (-1.0) /. 4.0 -. acos (dx /. nd) in
  let (cos, sin) = (cos angle, sin angle) in

  let acc = ref Float.infinity
  and approx = ref (n 0, n 0) in
  for i = 1 to 100 do
    for j = 1 to i - 1 do
      let a = i * i - j * j
      and b = 2 * i * j
      and c = i * i + j * j
      in

      let ac = div_num (n a) (n c)
      and bc = div_num (n b) (n c) in
      let dcos_ac = Float.abs @@ cos -. (f ac)
      and dcos_bc = Float.abs @@ cos -. (f bc) in

      if Float.min dcos_ac dcos_bc < !acc
      then begin
        acc := Float.min dcos_ac dcos_bc;
        approx := if dcos_ac < dcos_bc then (ac, bc) else (bc, ac)
      end
    done
  done;

  let (acos, asin) = !approx in
  let asin = asin */ (n (Sign.to_int @@ Float.sign_exn sin)) in
  printf "approximating sin(-a)=%.2f with %s=%.2f\n" sin (s asin) (f asin);
  !approx

let rot_id = mk_rot (n 1) (n 0)


let find_rotation skeleton =
  let norm2 (a, b) = let d = Vertex.sub b a in Vertex.dot d d in
  let ls_opt = List.max_elt skeleton ~cmp:(fun s1 s2 ->
      compare_num (norm2 s1) (norm2 s2))
  in match ls_opt with
  | Some (a, b) ->
    let (cos, sin) = closest_angle (a, b) in
    if cos */ cos +/ sin */ sin =/ n 1
    then
      let rot_fwd = mk_rot cos sin in
      let rot_bwd = mk_rot cos (n 0 -/ sin) in
      (rot_fwd, rot_bwd)
    else
      (rot_id, rot_id)
  | None -> (rot_id, rot_id)


let solve_inner skeleton =
  printf "solving [%s]\n"
    (String.concat ~sep:"," @@ List.map skeleton ~f:Segment.show);
  let (rot_fwd, rot_bwd) = find_rotation skeleton in
  let skeleton = List.map skeleton ~f:(fun (a, b) -> (rot_fwd a, rot_fwd b)) in
  let vs = List.concat_map skeleton ~f:(fun (a, b) -> [a; b]) in
  let (x_min, x_max) = Internal.min_max
      ~compare:compare_num (List.map vs ~f:fst)
  and (y_min, y_max) = Internal.min_max
      ~compare:compare_num (List.map vs ~f:snd)
  in
  let w = max_num (x_max -/ x_min) (div_num (n 1) (n 2))
  and h = max_num (y_max -/ y_min) (div_num (n 1) (n 2)) in
  let f (x, y) = (x +/ x_min, y +/ y_min) in
  let r = if w <=/ n 1 && h <=/ n 1
    then
      let (before, facets, after) = at_origin w h in
      if List.contains_dup before ~compare:Vertex.compare
      then None
      else Some (before, facets, after)
    else None
  in

  let (a, b, c) = Option.value r ~default:uber_stupid in
  (a, b, List.map c ~f:(Fn.compose rot_bwd f))


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
  let (b, f, a) = solve_inner skel in
  string_of_solution b f a
