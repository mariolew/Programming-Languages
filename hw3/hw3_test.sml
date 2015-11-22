(*Simple tests. All test cases should be evaluate to true*)

val test1 = only_capitals ["defense", "Foo", "Mario", "loser"] = ["Foo", "Mario"]

val test2 = longest_string1 ["a", "aaa", "fff", "d"] = "aaa"
val test3 = longest_string1 [] = ""

val test4 = longest_string2 ["a", "aaa", "fff", "d"] = "fff"
val test5 = longest_string3 ["a", "aaa", "fff", "d"] = "aaa"
val test6 = longest_string4 ["a", "Aaa", "FFF", "d"] = "FFF"

val test7 = longest_capitalized ["a", "aaa","Aaa", "FFF", "d"] = "Aaa"

val test8 = rev_string "mario" = "oiram"

val test9 = ((first_answer (fn x => if x = 1 then SOME x else NONE) []); false)
            handle NoAnswer => true
val test10 = first_answer (fn x => if x > 1 then SOME x else NONE) [1, 2, 3, 4] = 2
val test11 = all_answers  (fn x => if x > 1 then SOME [x] else NONE) [2, 3, 4] = SOME [2, 3, 4]

val test12 = count_wildcards (TupleP [Wildcard, Wildcard, UnitP]) = 2

val test13 = count_wild_and_variable_lengths (TupleP [Wildcard, Wildcard, UnitP, Variable "sss"]) = 5
val test14 = count_some_var("sss", TupleP [Wildcard, Variable "sss", Variable "sss", Variable "s"]) = 2

val test15 = check_pat (TupleP [Variable "f", Variable "f", Variable "f"]) = true
val test16 = check_pat (TupleP [Variable "s", Variable "f", Variable "f"]) = false

val test17 = match(Unit, UnitP) = SOME []
val test18 = match(Unit, Variable "sss") = SOME [("sss", Unit)]

val test19 = first_match Unit [ConstP(1), Variable "ddd"] = SOME [("ddd", Unit)]
                                                                               
