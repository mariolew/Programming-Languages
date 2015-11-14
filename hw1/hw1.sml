(* first *)
fun is_older (date1 : int * int * int, date2 : int * int * int) =
  #1 date1 < #1 date2 orelse
  (#1 date1 = #1 date2 andalso #2 date1 < #2 date2) orelse
  (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)


(* second *)
fun number_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else
      if #2 (hd dates) = month
      then 1 + number_in_month(tl dates, month)
      else number_in_month(tl dates, month)


(* third *)
fun number_in_months (dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* forth *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else
      if #2 (hd dates) = month
      then (hd dates)::dates_in_month(tl dates, month)
      else dates_in_month(tl dates, month)

(* fifth *)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* sixth *)
fun get_nth (slist : string list, n : int) =
  if n = 1
  then hd slist
  else get_nth(tl slist, n - 1)

(* seventh *)
fun date_to_string (date : (int * int * int)) =
  let
      val s = ["January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December"]
  in
      get_nth(s, #2 date) ^ " " ^ Int.toString(#1 date) ^ ", " ^ Int.toString(#3 date)
  end

(* eighth *)
fun number_before_reaching_sum (sum : int, ilist : int list) =
  let
      fun helper (sum : int, ilist : int list, ac : int) =
        if (hd ilist) + ac >= sum
        then 0
        else 1 + helper (sum, tl ilist, hd ilist + ac)
  in
      helper (sum, ilist, 0)
  end

(* ninth *)
fun what_month (day : int) =
  let
      val mon = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      number_before_reaching_sum(day, mon)
  end

(* tenth *)
fun month_range (day1 : int, day2 : int) =
  let
      fun count_from_to (from : int, to : int) =
        if from > to
        then []
        else from::count_from_to(from + 1, to)
  in
      count_from_to (what_month(day1), what_month(day2))
  end

(* eleventh *)
fun oldest (dates : (int * int * int) list) =
  let
      fun helper (dates : (int * int * int) list, date : (int * int * int)) =
        if null dates
        then date
        else
            if is_older(hd dates, date)
            then helper(tl dates, hd dates)
            else helper(tl dates, date)
  in
      if null dates
      then NONE
      else SOME(helper(tl dates, hd dates))
  end

  
(* Here comes the challenges *)

fun remove_duplicate (mons : int list) = 
  let
      fun num_in_list (num : int, ilst: int list) = 
        if null ilst
        then false
        else
            if num = hd ilst
            then true
            else num_in_list(num, tl ilst)
      fun judge_and_concat (orig : int list, res : int list) = 
        if null orig
        then res
        else 
            if num_in_list(hd orig, res)
            then judge_and_concat(tl orig, res)
            else judge_and_concat(tl orig, res @ [hd orig])
  in 
      judge_and_concat(mons, [])
  end
  
  
  
fun number_in_months_challenge (dates : (int * int * int) list, months : int list) = 
  let
      val re_mon = remove_duplicate(months)
  in
      number_in_months(dates, re_mon)
  end
  
fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) = 
  let
      val re_mon = remove_duplicate(months)
  in
      dates_in_months(dates, re_mon)
  end
  
  
  
(* challenge 2 *)

fun reasonable_date (date : (int * int * int)) = 
  let
      val mon = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      val monL = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      fun isLeap(date : (int * int * int)) = 
        #1 date mod 400 = 0 orelse (#1 date mod 4 = 0 andalso #1 date mod 100 <> 0)
      
      fun yearAndmonOK (date : (int * int * int)) = 
        #1 date > 0 andalso #2 date > 0 andalso #2 date < 13
      
      fun get_mon (month : int list, n : int) = 
        if n = 1
        then hd month
        else get_mon(tl month, n - 1)
  in
      if isLeap(date)
      then yearAndmonOK(date) andalso (#3 date) <= get_mon(monL, #2 date) andalso #3 date > 0
      else yearAndmonOK(date) andalso (#3 date) <= get_mon(mon, #2 date) andalso #3 date > 0
  end