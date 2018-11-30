
fun fold f acc xs =
    case xs of
      [] => []
    | x::xs' => fold f (f(acc, x)) xs'

val sum = fold (fn (y,x) => y + x) 0

(* Defining sum as a partially applied fold function *)

fun range i j = if i > j then [] else i::range (i+1) j

val countup i = range 1

fun exists predicate xs =
  case xs of 
    [] => false
  | x::xs' => predicate x orelse exists predicate xs'

val haszero = exists (fn x => x = 0)

val incrementAll = List.map (fn x => x + 1)

val removeZeros = List.filter (fn x => x <> 0)

(* Warning about 'value-restriction' *)
val pairWithOne = List.map (fn x=> (x,1))
(* Won't work because of polymorphic datatype *)
(* Workaround *)

fun pairWithOne xs = List.map (fn x=> (x,1)) xs
(* or explicit definition *)
val pairWithOne:string list -> (string * int) list = List.map (fn x=> (x,1))

