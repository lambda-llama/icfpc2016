open Core_kernel.Std

let min_max ~compare xs =
  (Option.value_exn (List.min_elt ~cmp:compare xs),
   Option.value_exn (List.max_elt ~cmp:compare xs))

let flip (x, y) = (y, x)
[@@inline]
