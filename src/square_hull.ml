open Num
open Core.Std
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


let centroid poly =
    let (sum_x, sum_y) = List.fold 
            ~init:(num_of_int 0, num_of_int 0) 
            ~f:(fun (sum_x, sum_y) (x, y) -> (sum_x +/ x, sum_y +/ y))
            poly
    and size = num_of_int @@ List.length poly
    in (div_num sum_x size, div_num sum_y size)


let update_curs ~cmp (cur_x, cur_y) (x, y) =
    ((if cmp cur_x x then cur_x else x),
     (if cmp cur_y y then cur_y else y))


let edges_of_poly poly =
    let (min_x, min_y) = Option.value_exn (List.reduce ~f:(update_curs ~cmp:(</)) poly)
    and (max_x, max_y) = Option.value_exn (List.reduce ~f:(update_curs ~cmp:(>/)) poly)
    in (min_x, min_y, max_x, max_y)


let edges_of_polys polys =
    let poly_edges = List.map ~f:edges_of_poly polys
    and update_edges (min_x, min_y, max_x, max_y) (cur_min_x, cur_min_y, cur_max_x, cur_max_y) =
        let (new_min_x, new_min_y) = update_curs ~cmp:(</) (min_x, min_y) (cur_min_x, cur_min_y)
        and (new_max_x, new_max_y) = update_curs ~cmp:(>/) (max_x, max_y) (cur_max_x, cur_max_y)
        in (new_min_x, new_min_y, new_max_x, new_max_y)
    in Option.value_exn (List.reduce ~f:update_edges poly_edges)


let correct_segment ((x1, y1), (x2, y2)) =
    if x1 = x2 then
        if y1 >/ y2 then ((x2, y2), (x1, y1))
        else ((x1, y1), (x2, y2))
    else 
        if x1 >/ x2 then ((x2, y2), (x1, y1))
        else ((x1, y1), (x2, y2))


let line_parameters line =
    let ((x1, y1), (x2, y2)) = correct_segment line in
    let m = div_num (y2 -/ y1) (x2 -/ x1) in
    let b = y1 -/ m */ x1 in 
    (m, b)


let line_function line =
    let (m, b) = line_parameters line
    in fun (x, y) -> y -/ (m */ x +/ b)


let line_function_explicit line =
    let (m, b) = line_parameters line
    in fun x -> m */ x +/ b


let is_higher (((x1, y1), (x2, y2)) as line) ((x, y) as point) =
    if x1 = x2 then x </ x1 else
    line_function line point >/ num_of_int 0


let segments_are_identical (((x11, y11), (x12, y12))) (((x21, y21), (x22, y22))) =
    x11 = x21 && y11 = y21 && x12 = x22 && y12 = y22


let check_vertical (((x1, _), (x2, _))) =
    if x1 = x2 then true
    else false


let handle_vertical_line (((x1, y1), (x2, y2)) as segment) (((x1_l, y1_l), (x2_l, y2_l)) as line) =
    if x1 <=/ x1_l && x2 >=/ x1_l then Some (x1_l, line_function_explicit segment x1_l)
    else None


let handle_vertical_segment (((x1, y1), (x2, y2)) as segment) (((x1_l, y1_l), (x2_l, y2_l)) as line) =
    let line_func_expl = line_function_explicit line in
    if line_func_expl x1 </ y1 || line_func_expl x1 >/ y2 then None
    else Some (x1, line_func_expl x1)


let segment_line_intersection (((x1, y1), (x2, y2)) as segment) (((x1_l, y1_l), (x2_l, y2_l)) as line) = 
    let segment = correct_segment segment
    and line = correct_segment line in
    if check_vertical line then
        if check_vertical segment then None
        else handle_vertical_line segment line
    else if check_vertical segment then
        if check_vertical line then None
        else handle_vertical_segment segment line
    else
        let (m1, b1) = line_parameters segment in
        let (m2, b2) = line_parameters line in
        if m1 = num_of_int 0 && m2 = num_of_int 0 then None
        else
            let intersection = div_num (b2 -/ b1) (m1 -/ m2) in
            let xmin = min x1 x2 in
            let xmax = max x1 x2 in
            let line_func1 = line_function_explicit line in
            if intersection <=/ xmax && intersection >=/ xmin 
                then Some (intersection, line_func1 intersection)
                else None


let ratio_between (x1, y1) (x2, y2) (x, y) =
    let two = num_of_int 2 in
    let dist1 = square_num ((x -/ x1) **/ two +/ (y -/ y1) **/ two)
    and dist  = square_num ((x1 -/ x2) **/ two +/ (y1 -/ y2) **/ two) in
    div_num dist1 dist


let get_point_coord : (num, (num * num)) List.Assoc.t -> ?equal:(num -> num -> bool) -> num ->  num * num =
    List.Assoc.find_exn;;


let remove_edge edge_list edge =
    List.filter ~f:(fun cur_edge -> cur_edge != edge) edge_list


let add_edge edge_list edge = edge :: edge_list


let create_vertice vertices location = 
    let new_id = List.length vertices |> num_of_int in
    (new_id, List.Assoc.add vertices new_id location)

    
let get_edge_segment vertices (id1, id2) = (get_point_coord vertices id1, get_point_coord vertices id2)


let sort_intersections = List.sort ~cmp:(fun (_, (x1, y1)) (_, (x2, y2)) -> 
    let compare_x = compare_num x1 x2 in
    if compare_x = 0 then compare_num y1 y2
    else compare_x)


let intersecting_edges (vertices, _, edge_list) line =
    List.map ~f:(fun edge -> 
        let edge_segment = get_edge_segment vertices edge in
        (edge, segment_line_intersection edge_segment line))
    edge_list |> List.filter_map ~f:(function
        | (edge, Some inter) -> Some (edge, inter)
        | (edge, None) -> None) |> sort_intersections





let replace_edge (vertices, origins, edge_list) ((id1, id2) as edge, inter) prev =
    let ((x1_origin, y1_origin), (x2_origin, y2_origin)) = get_edge_segment origins edge
    and ((x1_cur, y1_cur), (x2_cur, y2_cur)) = get_edge_segment vertices edge in
    let (x, y) = inter in
    let t = if compare_num x2_cur x1_cur != 0 then begin
        Printf.printf "on xs %s %s\n" (string_of_num x1_cur) (string_of_num x2_cur);
        div_num (x -/ x1_cur) (x2_cur -/ x1_cur) 
    end
    else begin
        Printf.printf "on ys %s" (string_of_num (y2_cur -/ y1_cur));
        div_num (y -/ y1_cur) (y2_cur -/ y1_cur) 
    end
    in
    let inter_origin = ((x1_origin +/ (x2_origin -/ x1_origin) */ t), (y1_origin +/ (y2_origin -/ y1_origin) */ t)) in
    let (id, vertices) = create_vertice vertices inter in
    let (_, origins) = create_vertice origins inter_origin in
    let edge_list = remove_edge edge_list edge in
    let edge_list = add_edge edge_list (id, id1) in
    let edge_list = add_edge edge_list (id, id2) in
    let edge_list = match prev with
        | None -> edge_list
        | Some id_prev -> add_edge edge_list (id, id_prev)
    in (id, (vertices, origins, edge_list))


let fold_once ((vertices, origins, edge_list) as fold) (line, higher) =
    let inters = intersecting_edges fold line in
    let fst_inter :: rest = inters in
    let (fst_id, fold) = replace_edge fold fst_inter None in
    let rec go prev cur_fold = function
        | [] -> cur_fold
        | inter :: rest ->
                let (cur, new_fold) = replace_edge cur_fold inter (Some prev)
                in go cur new_fold rest
    in go fst_id fold rest


let num_of_int_tuple (x, y) = (num_of_int x, num_of_int y)


let create_initial_fold ((polys, _)) =
    let (min_x, min_y, max_x, max_y) = edges_of_polys polys in
    let one = num_of_int 1 in
    let num_of_int_tuple (x, y) = (num_of_int x, num_of_int y) in
    let init_vertices = [
        (* 0 *)(min_x, min_y);
        (* 1 *)(min_x +/ one, min_y);
        (* 2 *)(min_x +/ one, min_y +/ one);
        (* 3 *)(min_x, min_y +/ one);
        (* 4 *)(max_x, max_y);
        (* 5 *)(min_x, max_y);
        (* 6 *)(max_x, min_y +/ one);
        (* 7 *)(min_x +/ one, max_y);
        (* 8 *)(max_x, min_y);
    ] in
    let init_polys = [
        List.map ~f:num_of_int [0; 5; 4; 8];
        List.map ~f:num_of_int [5; 3; 6; 4];
        List.map ~f:num_of_int [4; 6; 2; 7];
        List.map ~f:num_of_int [8; 4; 7; 1]
    ] in
    let init_destinations = [
        (* 0 *)(min_x, min_y);
        (* 1 *)(max_x -/ (min_x +/ one -/ max_x), min_y);
        (* 2 *)(max_x -/ (min_x +/ one -/ max_x), max_y -/ (min_y +/ one -/ max_y));
        (* 3 *)(min_x, max_y -/ (min_y +/ one -/ max_y));
        (* 4 *)(max_x, max_y);
        (* 5 *)(min_x, max_y);
        (* 6 *)(max_x, max_y -/ (min_y +/ one -/ max_y));
        (* 7 *)(max_x -/ (min_x +/ one -/ max_x), max_y);
        (* 8 *)(max_x, min_y)
    ] 
    in (init_vertices, init_polys, init_destinations)


let lines_of_fold (vertices, polys, destinations) =
    let fmt_coord = format_of_string "%s,%s"
    and string_of_poly poly = (num_of_int (List.length poly) :: poly) |> List.map ~f:string_of_num |> String.concat ~sep:" "
    in List.concat [
        [string_of_int (List.length vertices)];
        List.map ~f:(fun (x, y) -> Printf.sprintf fmt_coord (string_of_num x) (string_of_num y)) vertices;
        [List.length polys |> string_of_int];
        List.map ~f:string_of_poly polys;
        List.map ~f:(fun (x, y) -> Printf.sprintf fmt_coord (string_of_num x) (string_of_num y)) destinations;
    ]


let output_initial_fold_of_problem filename problem = 
    create_initial_fold problem |> lines_of_fold |> Out_channel.write_lines filename


let input_problem filename = In_channel.read_all filename |> problem_of_string


let vertices = ((), []) |>
    (fun (_, vertices) -> create_vertice vertices (num_of_int 0, num_of_int 0)) |>
    (fun (_, vertices) -> create_vertice vertices (num_of_int 0, num_of_int 1)) |>
    (fun (_, vertices) -> create_vertice vertices (num_of_int 1, num_of_int 1)) |>
    (fun (_, vertices) -> create_vertice vertices (num_of_int 1, num_of_int 0)) |>
    (fun (_, vertices) -> vertices)


let origins = vertices


let edges = List.map ~f:num_of_int_tuple [(0, 1); (1, 2); (2, 3); (3, 0)]


let init_fold = (vertices, origins, edges)


let line = (num_of_string "0", num_of_string "1/2"), (num_of_string "1", num_of_string "1/2")
let es0 = List.nth_exn edges 0 |> get_edge_segment vertices
let es1 = List.nth_exn edges 1 |> get_edge_segment vertices
let es2 = List.nth_exn edges 2 |> get_edge_segment vertices
let es3 = List.nth_exn edges 3 |> get_edge_segment vertices


(* let edges_of_problem problem = *)
    (* let vertices = fst problem in *)


