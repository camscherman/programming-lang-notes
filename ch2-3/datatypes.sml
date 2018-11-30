datatype my_type = TwoInts of int * int
                  | Str of string
                  | Pizza

val a = Str "a"
val b = Str
val c = Pizza
val d = TwoInts (1+2, 3+4)
val e = a

(* Case expressions *)

fun f(x: my_type) =
  case x of
    Pizza => 2
    | Str s => 8
    | TwoInts (i1, i2) => i1 + i2

    (* Datatype examples *)

datatype suit = Club | Spade | Heart | Diamond
datatype rank = Jack | Queen | King | Ace | Num of int

(* Defining an expression tree *)

datatype exp = Contant of int
              | Negate of exp
              | Add of exp * exp
              | Multiply of exp * exp

fun eval e =
  case e of
    Contant i => i  
    | Negate e => ~ (eval e)
    | Add(e1, e2) => (eval e1) + (eval e2)
    | Multiply(e1, e2) => (eval e1) * (eval e2)

fun number_of_adds e =
  case e of 
    Constant i => 0
    | Negate e2 => number_of_adds(e2)
    | Add(e1,e2) => 1 + number_of_adds(e1) + number_of_adds(e2)
    | Multiply(e1, e2) => number_of_adds(e1) + number_of_adds(e2)

