fun sum_triple triple =
    case triple of
      (x, y, z) => x + y + z

(* Bad style but explains how it works *)

fun full_name r =
    case r of
    {first = x, middle = y, last = z} =>
      x ^ " " ^ y ^ " " ^ z

(* Better style *)

fun sum_triple triple =
  let val (x,y,z) = triple
  in
    x + y + z
  end

fun full_name r =
  let val {first = x, middle = y, last = z }
  in 
    x ^ " " ^ y ^ " " ^ z
  end

(* Even better *)

fun sum_triple (x,y,z) =
    x + y + z

fun full_name {first = x, middle = y, last = z}
  x ^ " " ^ y ^ " " ^ z

fun rotate_left (x,y,z) =
    (y,z,x)

fun rotate_right t =
  rotate_left(rotate_left t)