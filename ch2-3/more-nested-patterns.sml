(* Old way *)
fun nondecreasing_old xs =
  case xs of
    [] => true
    | x::xs' => case xs' of
                  [] => true
                | y::ys' => x <= y andalso nondecreasing_old xs'

(* Better *)
fun nodecreasing xs =
  case xs of
  [] => true
  | _::[] => true
  | x::(neck::rest) => head <= neck 
                      andalso nondecreasing (neck::rest) 

datatype sgn = P | N | Z
fun multsign (x1, x2) =
  let fun sign x = if x = 0 then Z else if x > 0 then P else N
  in
    case (sign x1, sign x2) of
    (Z,_) => Z
    |(_,Z)=> Z
    |(P,P)=> P
    |(N,N)=> P
    |_ => N 
  endß