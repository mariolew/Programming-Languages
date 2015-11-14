(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(*part 1 a*)
fun all_except_option (str, slst) =
  case slst of
      [] => NONE
   | s::tail => if same_string(s, str)
                then SOME tail
                else case all_except_option(str, tail) of
                         NONE => NONE
                       | SOME t => SOME (s::t)
(*part 2 b*)
fun get_substitutions1 (sllst, s) =
  case sllst of
         [] => []
       | slst::tail => case all_except_option(s, slst) of
                           NONE => get_substitutions1(tail, s)
                         | SOME l => get_substitutions1(tail, s)@l


(*part 1 c*)
fun get_substitutions2 (sllst, s) =
  let fun helper (sllst, s, res) =
        case sllst of
            [] => res
         | slst::tail => case all_except_option(s, slst) of
                             NONE => helper(tail, s, res)
                           | SOME l => helper(tail, s, res@l)
  in helper(sllst, s, [])
  end

(*part 1 d*)
fun similar_names (sllst, name) =
  let val {first=x, middle=y, last=z} = name
      fun gen_names (sublist) =
        case sublist of
            [] => [name]
          | head::tail => {first=head, middle=y, last=z}::gen_names(tail)
  in gen_names(get_substitutions2(sllst, x))
  end

(*part 2 a*)
fun card_color (Color, Rank) =
  case Color of
      Spades => Black
    | Clubs => Black
    | _ => Red

(*part 2 b*)
fun card_value (Color, Rank) =
  case Rank of
      Num x => x
    | Ace => 11
    | _ => 10

(*part 2 c*)
fun remove_card (cs, c, e) =
  case cs of
      [] => raise e
    | cc::tail => if cc = c
                  then tail
                  else cc::remove_card(tail, c, e)

(*part 2 d*)
fun all_same_color (card_list) =
  case card_list of
      [] => true
    | c::[] => true
    | c1::c2::tail =>
      card_color(c1) = card_color(c2) andalso all_same_color(c2::tail)


(*part 2 e*)
fun sum_cards (card_list) =
  let
      fun sum_helper (card_list, acc) =
        case card_list of
            [] => acc
          | c::tail => sum_helper(tail, acc + card_value(c))
  in
      sum_helper(card_list, 0)
  end

(*part 2 f*)
fun score (card_list, goal) =
  let
      val s = sum_cards(card_list)
      fun score_helper (sum, goal) =
        if sum > goal
        then 3 * (sum - goal)
        else goal - sum
  in
      if all_same_color(card_list)
      then score_helper(s, goal) div 2
      else score_helper(s, goal)
  end

(*part 2 g*)
fun officiate (card_list, move_list, goal) =
  let
      fun of_helper (card_list, move_list, goal, held_list) =
        case move_list of
            [] => score(held_list, goal)
          | (Discard c)::m_tail => 
            of_helper(card_list, m_tail, goal, remove_card(held_list, c, IllegalMove))
          | Draw::m_tail => case card_list of
                                [] => score(held_list, goal)
                              | c::tail => if sum_cards(c::held_list) > goal
                                           then score(c::held_list, goal)
                                           else of_helper(tail, m_tail, goal, c::held_list)
  in
      of_helper(card_list, move_list, goal, [])
  end


(*challenge a*)
fun lst_min (lst) =
  case lst of
      [] => ~1
    | x::[] => x
    | h::t => Int.min(h, lst_min(t))

fun score_challenge (card_list, goal) =
  let
      val s = sum_cards(card_list)
      fun ace_count (card_list, acc) =
        case card_list of
            [] => acc
          | (_, r)::tail => if r=Ace then ace_count(tail, acc+1)
                       else ace_count(tail, acc)
      fun score_helper (sum, acc, goal, slst) =
        if acc < 0 then slst
        else
            if sum > goal
            then score_helper(sum - 10, acc - 1, goal, (3 * (sum - goal))::slst)
            else score_helper(sum - 10, acc - 1, goal, (goal - sum)::slst)
  in
      if all_same_color(card_list)
      then lst_min(score_helper(s, ace_count(card_list, 0), goal, [])) div 2
      else lst_min(score_helper(s, ace_count(card_list, 0), goal, []))
  end

fun officiate_challenge (card_list, move_list, goal) =
  let
      fun of_helper (card_list, move_list, goal, held_list) =
        case move_list of
            [] => score_challenge(card_list, goal)
          | (Discard c)::m_tail => 
            of_helper(card_list, m_tail, goal, remove_card(held_list, c, IllegalMove))
          | Draw::m_tail => case card_list of
                                [] => score_challenge(held_list, goal)
                              | c::tail => if sum_cards(c::held_list) > goal
                                           then score_challenge(c::held_list, goal)
                                           else of_helper(tail, m_tail, goal, c::held_list)
  in
      of_helper(card_list, move_list, goal, [])
  end




(*challenge b*)
fun should_discard (held, next, goal) =
  case held of
      [] => NONE
    | head::tail => if score_challenge(next::tail, goal) = 0 then
                        SOME head
                    else should_discard(tail, next, goal)
      
fun careful_player (card_list, goal) =
  let
      fun careful_player_helper (card_list, goal, held_list, mlst) =
        if score_challenge(held_list, goal) = 0 then mlst else
        if goal - 10 > sum_cards(held_list) then
            case card_list of
                [] => mlst@[Draw]
              | c::tail => careful_player_helper(tail, goal, c::held_list, mlst@[Draw])
        else case card_list of
                 [] => mlst
               | c::tail => if sum_cards(c::held_list) > goal then
                                case should_discard(held_list, c, goal) of
                                    NONE => mlst
                                  | SOME dis_card => mlst@[Discard dis_card, Draw]
                            else careful_player_helper(tail, goal, c::held_list, mlst@[Draw])
  in
      careful_player_helper(card_list, goal, [], [])
  end
