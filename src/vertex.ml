open Num
open Core_kernel.Std

open Internal

type t = coord * coord [@@deriving show]

let to_string (x, y) = string_of_num x ^ "," ^ string_of_num y
and of_string s = match String.split_on_chars ~on:[','] s with
| [x; y] -> (num_of_string x, num_of_string y)
| _other -> failwith ("Vertex.of_string: invalid vertex: " ^ s)

let sexp_of_t = Fn.compose sexp_of_string to_string
and t_of_sexp = Fn.compose of_string string_of_sexp

let compare = Tuple2.compare ~cmp1:compare_num ~cmp2:compare_num
and hash = Hashtbl.hash

let eq (x1, y1) (x2, y2) = eq_num x1 x2 && eq_num y1 y2
[@@inline]

let neq x1y1 x2y2 = not (eq x1y1 x2y2)

let sub (x1, y1) (x2, y2) = (x1 -/ x2, y1 -/ y2)
[@@inline]

let add (x1, y1) (x2, y2) = (x1 +/ x2, y1 +/ y2)
[@@inline]

let dot (x1, y1) (x2, y2) = x1 */ x2 +/ y1 */ y2
[@@inline]

let scale f (x, y) = (x */ f, y */ f)
[@@inline]

module Vertex3 = struct
  type nonrec t = t * t * t

  let sexp_of_t _ = stub "Vertex3.sexp_of_t"
  and t_of_sexp _ = stub "Vertex3.t_of_sexp"

  let compare = Tuple3.compare ~cmp1:compare ~cmp2:compare ~cmp3:compare
  and hash = Hashtbl.hash
end

(** Reflects a coordinate agains a line segment [a, b]. *)
let reflect: t -> (t * t) -> t =
  let go ((x, y), a, b) =
    let (dx, dy) = sub b a in
    let dx2 = dx */ dx
    and dy2 = dy */ dy in
    let nd = dx2 +/ dy2 in
    let u = div_num (dx2 -/ dy2) nd
    and v = div_num (n 2 */ dx */ dy) nd in

    (* http://math.stackexchange.com/q/65503/113653 *)
    let (ax, ay) = a in
    (u */ (x -/ ax) +/ v */ (y -/ ay) +/ ax,
     v */ (x -/ ax) -/ u */ (y -/ ay) +/ ay)
  in

  let hashable = Hashtbl.Hashable.of_key(module Vertex3) in
  let memo = Memo.general ~hashable go in
  fun p (a, b) -> memo (p, a, b)
