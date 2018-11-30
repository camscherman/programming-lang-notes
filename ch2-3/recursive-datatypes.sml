datatype my_int_list = Empty
                      | Cons int * my_int_list

val x = Cons (4, Cons (5, Cons (2008, Empty)))

fun append_my_list (xs, ys) =
    case xs of
      Empty => ys
      | Cons(x, xs') => Cons (x, append_my_list(xs', ys))

(* Using options in pattern matching *)

fun int_or_zero_inclusive intoption =
        case intoption of
          None => 0
          | Some i => i 

fun sum_list xs =
    case xs of
      [] => 0
      | x::xs' => x + sum_list xs'

fun append(xs, ys) =
    case xs of
      [] => ys
      | x::xs' => x::append(xs', ys)

