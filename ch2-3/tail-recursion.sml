(* Factorial with and without an accumulator *)

fun factorial n =
  if n = 1  then n else n * factorial (n-1)

fun tail_factorial n =
    let fun aux (n, acc) =
        if n = 0 
        then acc
        else
          aux(n-1, acc*n)
    in
      aux(n, 1)
    end

(* Sum in tail recursive format *)

fun sum xs =
  let fun aux(xs,acc) =
    case xs of
      [] => acc
    | x::xs' => aux(xs', acc + x)
  in
    aux(xs,0)
  end

  (* Reverse *)

  (* normal reverse -- Inefficient *)

  fun rev xs =
      case [] => []
    | x::xs' => rev xs' @ x

  fun rev xs =
    let fun aux 