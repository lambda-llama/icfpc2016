open Num
open Core_kernel.Std

open Internal

module Vertex_tests = struct
  let () as _test_reflect = begin
    assert (Vertex.reflect (n 0, n 0) ((n 1, n 0), (n 1, n 1)) = (n 2, n 0));
    assert (Vertex.reflect (n 2, n 0) ((n 1, n 0), (n 1, n 1)) = (n 0, n 0))
  end
end

module Facet_tests = struct
  let () as _test_fix =
    let a = (n 0, n 0) in
    let b = (n 1, n 0) in
    let c = (n 2, n 0) in
    let d = (n 1, n 1) in
    let f = [(a, b); (b, c); (c, d); (d, a)] in
    assert (Facet.fix f = [(a, c); (c, d); (d, a)])

  let () as _test_merge =
    let a = (n 0, n 0)
    and b = (n 0, n 1)
    and c = (n 1, n 1)
    and d = (n 1, n 0)
    and e = (n 0, n 2)
    and f = (n 1, n 2)
    in let open Facet in begin
      assert ((merge
                 (create [(a, c); (c, b); (b, a)])
                 (create [(a, d); (d, c); (c, a)])) = create [
          (a, d); (d, c); (c, b); (b, a);
        ]);
      assert ((merge
                (create [(a, d); (d, c); (c, b); (b, a)])
                (create [(b, c); (c, f); (f, e); (e, b)])) = create [
          (a, d); (d, f); (f, e); (e, a);
        ])
    end

  let () as _test_reflect =
    let a = (n 0, n 0)
    and b = (n 0, n 1)
    and c = (n 1, n 1)
    and d = (n 1, n 0)
    in let open Facet in begin
      assert ((reflect
                (create [(a, b); (b, c); (c, d); (d, a)])
                (c, d)) = create [
          ((n 2, n 0), d); (d, c); (c, (n 2, n 1)); ((n 2, n 1), (n 2, n 0))
        ])
    end
end


module Figure_tests = struct
  let () as _to_unit_square_test =
    let (a, b, c, d) = ((n 0, n 0), (n 1, n 0), (n 1, n 1), (n 0, n 1)) in
    let unit_square = [
      Facet.create [(a, b); (b, c); (c, d); (d, a)]
    ] in

    let figeq a b = List.for_all2_exn a b ~f:Facet.eq
    and tus = Option.value_exn (Figure.to_unit_square unit_square)
    in assert (figeq unit_square (Figure.transform_figure tus unit_square))

  let () as _is_square_area_test =
    let a = (n 0, n 0)
    and b = (n 0, n 1)
    and c = (n 1, n 1)
    and d = (n 1, n 0) in

    let square = [Facet.create [(a, d); (d, c); (c, b); (b, a)]]
    and poly = [
      Facet.create [(a, d); (d, (n 2, n 2)); ((n 2, n 2), b); (b, a)]
    ] in begin
      assert (Figure.is_square_approx square);
      assert (Figure.area square = n 1);
      assert (not (Figure.is_square_approx poly));
      assert (Figure.area poly <> n 1);
    end

  let () =
    let up = ((n 0, n 0), (n 0, n 1)) in
    let down = ((n 0, n 2), (n 0, n 1)) in
    let left = ((n 0, n 1), (n ~-1, n 1)) in
    let right = ((n 0, n 1), (n  1, n 1)) in
    let ans1 = Figure.next_cc_segment up [left; right] in
    let ans2 = Figure.next_cc_segment down [left; right] in
    assert (ans1 = left);
    assert (ans2 = right)

  let () =
    let up = ((n 1, n 0), (n 1, n 1)) in
    let right = ((n 1, n 1), (n  0, n 1)) in
    let down_right = ((n 1, n 1), (n  0, n 0)) in
    let ans = Figure.next_cc_segment up [right; down_right] in
    assert (ans = down_right)

  let () =
    let h = div_num (n 1) (n 2) in
    let a = (n 0, n 0) in
    let b = (n 1, n 0) in
    let c = (h, h) in
    let d = (n 0, h) in
    let skel = [
        (a, b);
        (b, c);
        (c, d);
        (a, d);
        (a, c);
      ] in ignore @@ Figure.of_skeleton skel
end


module Engine_tests = struct
  let () as _test_search1 =
    let a = (n 0, n 0)
    and b = (n 0, div_num (n 1) (n 2))
    and c = (n 1, div_num (n 1) (n 2))
    and d = (n 1, n 0) in

    let skeleton = [(a, b); (b, c); (c, d); (d, a)] in
    let dst = Figure.of_skeleton skeleton in begin
      prerr_endline "== 1/2 square ==";
      prerr_endline @@ Engine.search dst
    end


  let () as _test_search2 =
    let h = div_num (n 1) (n 2) in
    let a = (n 0, n 0)
    and b = (n 0, h)
    and c = (h, h)
    and d = (n 1, n 0) in

    let skeleton = [(a, b); (b, c); (c, d); (d, a); (a, c)] in
    let dst = Figure.of_skeleton skeleton in begin
      prerr_endline "== spec. example ==";
      prerr_endline @@ Engine.search dst
    end
end

let run () = ()
