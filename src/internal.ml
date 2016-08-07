open Num
open Core_kernel.Std

type coord = Num.num [@printer fun fmt n -> fprintf fmt "%s" (string_of_num n)]
[@@deriving show]

let n x = num_of_int x
[@@inline]

let s x = string_of_num x
[@@inline]

let min_max ~compare xs =
  (Option.value_exn (List.min_elt ~cmp:compare xs),
   Option.value_exn (List.max_elt ~cmp:compare xs))

let flip (x, y) = (y, x)
[@@inline]

let stub s = failwith (sprintf "%s: not implemented" s)
