open Core_kernel.Std

let (</>) = Filename.concat

let () =
  match Sys.argv with
  | [|_binary; problem_id|] ->
    let in_path = "problems" </> sprintf "problem%s.txt" problem_id
    and out_path = "solutions" </> sprintf "solution%s.txt" problem_id
    in

    let s = In_channel.read_all in_path in
    let (_shade, skeleton) = Input.problem_of_string s in
    Out_channel.write_all out_path ~data:(Bf.bf_solve skeleton)
  | _other -> prerr_endline "usage: %prog PROBLEM_ID"
