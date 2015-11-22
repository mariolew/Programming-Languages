(* Dan Grossman, CSE341 Spring 2013, HW3 Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu
(*g takes a function f1 which takes an unit and returns an int
 and f2 which takes a string and returns an int and a pattern p
 generally g computes an int about string or wildcard
 from a given pattern*)
fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(*Function 1*)
val only_capitals  = List.filter (fn str => Char.isUpper(String.sub(str, 0)))

(*Function 2*)
(*fun longest_string1 slst =
  let
      fun bigger (s1, s2) =
        if String.size(s1) > String.size(s2)
        then s1
        else s2
  in
      foldl bigger "" slst
  end*)
val longest_string1 =
    foldl (fn (s1, s2) => if String.size(s1) > String.size(s2) then s1 else s2) ""


(*Function 3*)
val longest_string2 =
    foldl (fn (s1, s2) => if String.size(s1) >= String.size(s2) then s1 else s2) ""

(*Function 4*)
fun longest_helper f slist =
  foldl (fn (s1, s2) => if (f(String.size(s1), String.size(s2))) then s1 else s2) "" slist

val longest_string3 = longest_helper (fn (a, b) => if a > b then true else false) 

val longest_string4 = longest_helper (fn (a, b) => if a >= b then true else false)


(*Funtion 5*)
val longest_capitalized = longest_string1 o only_capitals

(*Function 6*)
val rev_string = implode o rev o explode

(*Funtion 7*)
fun first_answer f alist =
  case alist of
      [] => raise NoAnswer
    | head::tail => case f head of
                        NONE => first_answer f tail
                      | SOME x => x

(*Function 8*)
fun all_answers f alist =
  let
      fun all_helper (f, alist, acc) =
        case alist of
            [] => SOME acc
          | a::ax => case f a of
                         NONE => NONE
                       | SOME e => all_helper(f, ax, acc@e)
  in
      all_helper(f, alist, [])
  end

(*Funtion 9*)
val count_wildcards = g (fn () => 1) (fn s => 0)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

fun count_some_var (str, p) = g (fn () => 0) (fn s => if s = str then 1 else 0) p
                           
(*Funtion 10*)
fun check_pat p =
  let fun get_pattern_str p =
        case p of
            Variable x => [x]
          | TupleP ps => foldl (fn (a, b) => b @ (get_pattern_str a)) [] ps
          | ConstructorP(_, pp) => get_pattern_str pp

      fun is_repeated slist =
        case slist of
            [] => true
          | x::xs => List.exists (fn s => s = x) xs
  in
      is_repeated (get_pattern_str p)
  end

(*Function 11*)
fun match (v, p) =
  case (v, p) of
      (_, Wildcard) => SOME []
    | (vv, Variable s) => SOME [(s, vv)]
    | (Unit, UnitP) => SOME []
    | (Const i, ConstP j) => if i = j then SOME [] else NONE
    | (Tuple vs, TupleP ps) => if length vs = length ps then all_answers match (ListPair.zip(vs, ps))
                               else NONE
    | (Constructor(s1, v1), ConstructorP(s2,p2)) => if s1 = s2 then match(v1, p2) else NONE
    | _ => NONE

(*Function 12*)
fun first_match v plist =
  SOME (first_answer (fn p => match(v, p)) plist)
  handle NoAnswer => NONE
