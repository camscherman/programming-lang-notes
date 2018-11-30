fun silly (z: int) =
  let 
    val x = if z > 0 then z else 34
    val y = x + z + 9
  in
    if x > y then x * 2 else y * y
  end


fun silly2 () = 
  let
    val x = 1
  in 
    (let val x = 2 in x + 1 end) + (let val y = x + 2 in y + 1 end)
  end

(* Nested functions *)

fun count_up_from_one (last: int) =
  let
    fun count(first: int) =
      if first = last then
        first::[]
      else first :: count (first + 1) 
  in
    count(1)
  end

fun good_max (xs : int list) =
  if null xs 
  then 0
  else if null (tl xs) 
  then hd xs
  else 
    let
      val tail_max = good_max(tl xs)
    in 
      if hd xs > tail_max 
      then hd xs
      else tail_max 
    end

(* max using options *)
(* fn int list -> int option *)
fun max1(xs : int list) =
  if null xs
  then NONE 
  else 
    let val tl_ans = max1(tl xs)
    in if isSome tl_ans andalso valOf tl_ans > hd xs
      then tl_ans
      else SOME (hd xs)
    end

fun max2(xs: int list) =
  if null xs
  then NONE
  else let 
    fun max_non_empty(xs: int list) =
      if null (tl xs)
      then hd xs
      else let val tl_ans = max_non_empty(tl xs)
              in 
                if hd xs > tl_ans 
                then hd xs
                else tl_ans
              end
    in 
      SOME (max_non_empty xs)
    end 




