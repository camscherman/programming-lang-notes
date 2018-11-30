fun compose (f, g) = fn x => f(g x)

fun sqrt_of_abs i = Math.sqrt(Real.fromInt(abs i))

fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs) i 

fun sqrt_of_abs = Math.sqrt o Real.fromInt o abs

(* Can use a pipeline *)

fun sqrt_of_abs i = i |> abs |> Real.fromInt |> Math.sqrt 

(* Can also define our own infix operators *)

infix !>

fun x !> f = f x

fun sqrt_of_abs i = i !> abs !> Real.fromInt !> Math.sqrt

fun backup1 (f,g) = fn x => case f x of 
                            NONE => g x
                          | SOME y => y

fun backup2 (f,g) = fn x => f x handle _ => g x