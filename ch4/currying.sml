fun sorted_3_tuple (x,y,z) = if x <= y andalso y <= z then true else false

val sorted_3 = fn x => fn y => fn z => if x <= y andalso y <= z then true else false
(* Curried version *)

val t2 = ((sorted_3 3) 7) 9

fun sorted_3_nicer x y z = z >= y andalso y >= x 

(* Curried fold *)

fun fold f acc xs =
    case xs of
      [] => []
    | x::xs' => fold f (f(acc, x)) xs'

