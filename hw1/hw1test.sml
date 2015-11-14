val my_birth = (1993, 3, 11)
val his = (1992, 3, 13)
val her = (1993, 2, 13)
val his_bro = (1993, 3, 12)

val true1 = is_older(my_birth, his_bro)
val false1 = is_older(my_birth, his)
val false2 = is_older(my_birth, her)

val dates = [(1, 2, 3), (2, 4, 5), my_birth, his]
val two = number_in_month(dates, 3)
val one = number_in_month(dates, 2)

val three = number_in_months(dates, [2, 3])

val two_march = dates_in_month(dates, 3)
val empty = dates_in_months(dates, [])

val mar2_feb1 = dates_in_months(dates, [2, 3])

val str = ["January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December"]

val Mar = get_nth(str, 3)

val ilst = [1, 2, 3, 5, 6]

val t3 = number_before_reaching_sum(6, ilst)
val s3 = number_before_reaching_sum(7, ilst)

val Feb = what_month(33)

val feb2dec = month_range(33, 364)

val onetwothree = oldest(dates)

val pass = "All tests passed."

val cthree = number_in_months_challenge(dates, [2, 2, 2, 3])


val cmar2_feb1 = dates_in_months_challenge(dates, [2, 3, 3, 3])
    