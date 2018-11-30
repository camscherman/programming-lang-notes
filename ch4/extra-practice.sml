fun compose_opt f g =
  fn x =>
    let val r = g x
    in
      case r of 
        NONE => NONE
      | _ => f r
    end

fun add_one x =
  case x of 
    NONE => NONE
  | SOME i => SOME (i + 1)

fun add_two x =
  case x of 
    NONE => NONE
  | SOME i => SOME (i + 2)

val add_three = (compose_opt add_one add_two)

fun do_until f p x =
  let val b = p x in
    case b of 
      false => x
    | true => 
        let val c = f x in
          do_until f p c 
        end
  end

val remove_odd = do_until (fn x=> x div 2 )(fn x=> x mod 2 <> 1)

fun factorial x =
  let val factorial_pair = 
    do_until (fn (p, q) => ((p * q), q + 1)) (fn (p,q) => q < x )
  in 
    #1 (factorial_pair(1,1))
  end

fun merge xs ys =
  case xs of
      [] => ys
    | x::xs' =>
      case ys of
        []  => xs
      | y::ys' => 
        if x < y then x::y::(merge xs' ys') else y::x::(merge xs' ys')
  
fun countItems  xs  =
  let fun aux acc ys  =
    case ys of 
      [] => acc
    | y::ys' =>
      aux (acc+1) ys'
  in
    aux 0 xs
  end

fun split xs =
  let 
    val size = (countItems xs)
    val half = size div 2
    fun aux firstHalf secondHalf remain =
      case remain of
          0 => (firstHalf, secondHalf)
        | _ => 
          case secondHalf of 
              [] => (firstHalf,[])
            | p::ps => aux (firstHalf@[p]) ps (remain - 1)
  in
    aux [] xs half
  end

fun mergeSort xs =
  case xs of
      [] => []
    | [x] => [x]
    | _ => let val (first,second) = split xs
            in 
              merge (mergeSort first) (mergeSort second)
            end
     



                  


