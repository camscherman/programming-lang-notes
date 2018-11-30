(* Returning a function *)
fun double_or_triple f = 
  if f 7
  then fn x => 2*x
  else fn x => 3*x

datatype exp = Constant of int
              | Negate of exp
              | Add of exp*exp
              | Multiply of exp*exp

fun true_of_all_constants (f, e) =
      case e of
        Contant => f e 
      | Negate x =>  true_of_all_constants(f, x)
      | Add (x,y) => true_of_all_constants(f, x) 
                    andalso true_of_all_constants(f,y)
      | Multiply (x,y) => true_of_all_constants(f, x) 
                    andalso true_of_all_constants(f,y)

fun all_even e =
    true_of_all_constants((fn x=> x mod 2 = 0), e)