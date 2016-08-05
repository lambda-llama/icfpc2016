type expr =
  | Num of int
  | Add of expr * expr
[@@deriving show]


type problem_spec = polygon list

type polygon = vertext list

type vertex = (frac * frac)

type frac = (int * int)
(* 1 *)
(* 4 *)
(* 0,0 *)
(* 1,0 *)
(* 1/2,1/2 *)
(* 0,1/2 *)
(* 5 *)
(* 0,0 1,0 *)
(* 1,0 1/2,1/2 *)
(* 1/2,1/2 0,1/2 *)
(* 0,1/2 0,0 *)
(* 0,0 1/2,1/2 *)
