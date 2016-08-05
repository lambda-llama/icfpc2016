type coord = Num.num
    [@printer fun fmt n -> Format.pp_print_string fmt (Num.string_of_num n)]

and vertex = coord * coord

and segment = vertex * vertex

and poly = vertex list

and shade = poly list

and skeleton = segment list

and problem = shade * skeleton
[@@deriving show]
