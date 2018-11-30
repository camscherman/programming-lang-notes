(* This is how hd is defined *)

fun hd ls =
  case ls of
      [] => raise List.Empty
    | x::_ => x

(* Defining your own exceptions *)

exception MyUndesireableCondition
exceptions MyOtherError of int * int

fun mydiv (x,y) =
  if y = 0 
  then raise MyUndesireableCondition
  else x div y

fun max_list (xs,ex) =
  case xs of 
      [] => raise ex
    | x::[] => x 
    | x::xs' => Int.max(x, max_list (xs', ex))

(* More examples *)

fun sum xs =
  case xs of
    []  => 0
  | x::xs' => x + sum xs'

fun sum_tail 

