open Core_kernel.Std

type t = Vertex.t * Vertex.t [@@deriving show]

let to_string (a, b) = Vertex.to_string a ^ " " ^ Vertex.to_string b
and of_string s = match String.split_on_chars ~on:[' '] s with
| [a; b] -> (Vertex.of_string a, Vertex.of_string b)
| _other -> failwith ("Segment.of_string: invalid segment: " ^ s)

let sexp_of_t = Fn.compose sexp_of_string to_string
and t_of_sexp = Fn.compose of_string string_of_sexp

let compare = Tuple2.compare ~cmp1:Vertex.compare ~cmp2:Vertex.compare
and hash = Hashtbl.hash

let twin (a, b) = (b, a)

let eq (a, b) (c, d) = Vertex.eq a c && Vertex.eq b d
let eq_unordered (a, b) (c, d) =
  (Vertex.eq a c && Vertex.eq b d) ||
  (Vertex.eq a d && Vertex.eq b c)

let neq s1 s2 = not (eq s1 s2)
let neq_unordered s1 s2 = not (eq_unordered s1 s2)
