type expr =
  | Num of int
  | Add of expr * expr
[@@deriving show]
