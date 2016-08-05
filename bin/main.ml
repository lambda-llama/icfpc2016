open Core_kernel.Std

let () =
  let s = In_channel.(input_all stdin) in
  let p = Input.problem_of_string s in
  print_endline @@ Types.show_problem p
