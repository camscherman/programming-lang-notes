fun n_times(f,n,x) =
  if n = 0 
  then x
  else f (n_times(f, n-1, x))

fun increment x = x + 1
fun double x = x + x

fun add (n,x) = n_times(increment, n, x)
fun double_n_times (n,x) = n_times(double, n, x)
fun nth_tail(n, xs) = n_times(tl, n , xs)

(* Contrast *)
(* Higher order that isn't polymorphic *)
fun times_until_zero(f,x) =
  if x = 0 then 0 else 1 + times_until_zero(f, f x)

(* First order with polymorphic type *)

fun len xs =
  case xs of 
    [] => 0
  | _::xs' => 1 + len xs' 

  