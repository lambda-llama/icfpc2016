open Core_kernel.Std

open Types

let () =
  let s = In_channel.(input_all stdin) in
  let (_shade, skeleton) = Input.problem_of_string s in
  let dst = Figure.of_skeleton skeleton in begin
    prerr_endline @@ Figure.show dst;
    print_endline @@ Engine.search dst
  end
