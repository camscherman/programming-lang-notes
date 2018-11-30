datatype pass_fail = Pass | Fail
fun pass_fail rc =
  case rc of 
      { grade = SOME i, id = _ } => if i >= 75 then Pass else Fail
    | _ => Fail

fun has_passed rc =
  let val stat = pass_fail rc
  in 
    case stat of
        Pass => true
       | Fail => false
  end

fun number_passed xs =
  case xs of
      [] => 0
    | x::xs' => if has_passed x 
        then 1 + number_passed xs' 
        else number_passed xs'
fun number_misgraded xs =
  case xs of 
      [] => 0
    | (Fail,g)::xs' => if has_passed g then (1 + number_misgraded xs') else number_misgraded xs'
    | (Pass,g)::xs' => if has_passed g = false then (1 + number_misgraded xs') else number_misgraded xs' 

datatype 'a Tree = Leaf | Node of {value: 'a, left: 'a Tree, right: 'a Tree}
datatype flag = leave_me_alone | prune_me

fun tree_height t =
  let fun aux(p,acc) =
        case p of
          Leaf => acc + 1
        | Node {value = _, left = y, right = z} => 
          let 
            val left_height = aux(y, 1+acc)
            val right_height = aux(z, 1+acc)
          in
            Int.max (left_height, right_height)
          end
  in
    aux(t,0)
  end


fun sum_tree t =
  case t of
    Leaf => 0
  | Node {value = x, left = y, right = z} =>
    x + sum_tree y + sum_tree z

fun gardener  t = 
  case t of
    Leaf => Leaf
  | Node {value = prune_me, left = _ , right = _ } => Leaf
  | Node {value = leave_me_alone , left = y , right = z } => 
          Node {value = leave_me_alone, left = gardener y, right = gardener z } 

  (* List functions *)

  fun my_take (xs, i) =
    case i of
        0 => []
      | _ =>
        case xs of
            [] => raise Subscript
          | x::xs' => x::my_take(xs', (i - 1))     

  datatype nat = Zero | Succ of nat 

  fun is_positive nm =
    case nm of 
        Zero => false
      | Succ _ => true

  exception Negative; 
  fun pred nm =
    case nm of 
      Zero => raise Negative
    | Succ n => n 
  
  fun nat_to_int nat =
    let fun aux (acc,n) =
      case n of 
        Zero => 
          acc
      | Succ pred =>  
          aux(acc+1, pred)
    in 
        aux (0, nat)
    end

fun add (nat1, nat2) =
  case nat1 of
    Zero =>
      nat2
  | Succ pred =>
      add(pred, Succ nat2)

fun sub(nat1, nat2) =
  case nat2 of
    Zero => nat1
  | Succ p => sub(pred nat1, p)

fun mult(nat1, nat2) =
  case nat1 of
   Zero => Zero
  | Succ Zero => nat2
  | Succ n => mult(n, add(nat2, nat2))

fun less_than(nat1,nat2) =
  case (nat1,nat2) of
    (Zero, Zero) =>
      false
    | (Zero, _) => 
      true
    | (_, Zero) => 
      false
    | (Succ p, Succ q) =>
      less_than(p,q)

datatype intSet =
      Elems of int list
    | Range of {from: int, to: int}
    | Union of intSet * intSet
    | Intersection of intSet * intSet

fun isEmpty set =
  case set of 
   Elems [] => true
  | Elems _ => false
  | Range {from = x, to = y} => 
      (x = y)
  | Union (set1, set2) =>
      (isEmpty set1) andalso (isEmpty set2)
  | Intersection (set1, set2) =>
      (isEmpty set1) orelse (isEmpty set2)

fun contains (set,i) =
  case set of
    Elems [] => false
  | Elems x::xs' =>
      if x = i then true else contains((Elems xs'), i)
  | Range {from=x, to= y}=>
      if i >= x andalso i <= y then true else false
  | Union (s1, s2) =>
      (contains(s1, i)) orelse (contains(s2, i))
  | Intersection (s1, s2) =>
      (contains(s1, i)) andalso (contains(s2,i))



  