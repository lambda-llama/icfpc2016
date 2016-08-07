open Core_kernel.Std

(* True if OAB makes a clockwise turn. *)
let clockwise (o_x, o_y) (a_x, a_y) (b_x, b_y) = 
  (a_x -. o_x) *. (b_y -. o_y) -. (a_y -. o_y) *. (b_x -. o_x) <= 0.

(* Construct convex hull from a list of vertices. *)
let of_vertices pts =
  if List.length pts < 3 then pts
  else
    let sorted = List.sort ~cmp:compare pts in

    let rec clean l x =
        match l with
        | a :: (b :: _ as t) when clockwise b a x -> clean t x
        | _ -> l
    in
    
    let rec part_hull acc = function
      | [] -> acc
      | x::xs -> part_hull (x::(clean acc x)) xs
    in
    
    let lower = part_hull [] sorted in
    let upper = part_hull [] (List.rev sorted) in

    (List.rev (List.tl_exn lower)) @ (List.rev (List.tl_exn upper))
