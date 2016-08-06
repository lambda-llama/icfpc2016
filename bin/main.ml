open Core_kernel.Std

let () =
  let s = In_channel.(input_all stdin) in
  let p = Output.skeleton_to_facets s in
  print_endline @@  p
