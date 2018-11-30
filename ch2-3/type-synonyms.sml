datatype suit = Club | Diamond |Heart | Club
datatype rank = Ace | King | Queen | Jack | Num of int

type card = suit * rank
type name_record = {
                    student_num: int option,
                    first : string,
                    middle : string option,
                    last : string,
                    }

fun is_queen_of_spades(c1 : card) =
      #1 c1 = Spade andalso #2 c1 = Queen

val c1 : card = (Diamond, Ace)
val c2 : suit * rank = (Heart, Ace)
val c3  = (Spade, Ace)

