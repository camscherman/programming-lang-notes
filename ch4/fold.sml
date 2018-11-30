fun fold (f, acc, xs) =
    case xs of
      [] =>
    | x::xs' => fold(f, f(acc, x), xs')

(* Couple examples *)

fun f1 xs =
  fold(fn (acc,x) => acc + x, 0 , xs)
(* Sum all in list *)

fun f3 (xs, hi, lo) =
  fold((fn (acc,x)=> acc + (if x < hi andalso x > lo then 1 else 0)), 0 , xs)
(* Count numbers between hi and lo *)

fun f4 (xs, s) =
  let 
    val i = String.size s 
  in
    fold((fn (acc,x) =>  acc andalso String.size x < i) , true, xs)
  end
(* Every element greater than sample string size *)

fun f5 (g, xs) =
  fold((fn (acc, x)=> acc andalso g x), true, xs )


fun f4again (xs,s) =
  let 
    val i = String.size s
  in
    f5((fn x => String.size x < i), xs)
  end
