module Domain_error = Error
open Base

let is_leap year = year % 4 = 0 && (year % 100 <> 0 || year % 400 = 0)

let month_length year = function
  | 2 -> if is_leap year then 29 else 28
  | 4 | 6 | 9 | 11 -> 30
  | _ -> 31

let draw_date tc =
  let year = Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:9999 ()) in
  let month = Hegel.draw tc (Hegel.integers ~min_value:1 ~max_value:12 ()) in
  let day =
    Hegel.draw tc
      (Hegel.integers ~min_value:1 ~max_value:(month_length year month) ())
  in
  let text = Printf.sprintf "%04d-%02d-%02d" year month day in
  let date =
    Result.ok_or_failwith
      (Result.map_error (Date.of_string text) ~f:Domain_error.to_string)
  in
  assert (String.equal (Date.to_string date) text);
  date

let draw_zone tc =
  let names = Time.supported_zone_names in
  let index =
    Hegel.draw tc
      (Hegel.integers ~min_value:0 ~max_value:(List.length names) ())
  in
  let name =
    if index < List.length names then List.nth_exn names index
    else
      let minutes =
        Hegel.draw tc
          (Hegel.integers ~min_value:(-14 * 60) ~max_value:(14 * 60) ())
      in
      Printf.sprintf "%c%02d:%02d"
        (if minutes < 0 then '-' else '+')
        (Int.abs minutes / 60)
        (Int.abs minutes % 60)
  in
  Result.ok_or_failwith
    (Result.map_error (Time.parse_zone name) ~f:Domain_error.to_string)

let draw_instant tc =
  let midnight = Date.midnight_utc (draw_date tc) in
  let seconds =
    Hegel.draw tc
      (Hegel.integers ~min_value:0 ~max_value:(Time.seconds_per_day - 1) ())
  in
  Option.value_exn (Ptime.add_span midnight (Ptime.Span.of_int_s seconds))

let add_seconds time seconds = Ptime.add_span time (Ptime.Span.of_int_s seconds)

(* Proleptic Gregorian ordinal, independent of Ptime's span arithmetic. Year 0
   is a leap year; dividing the preceding shifted years counts it correctly. *)
let ordinal_ymd (year, month, day) =
  let prior_month_days =
    List.init (month - 1) ~f:(fun index -> month_length year (index + 1))
    |> List.fold ~init:0 ~f:( + )
  in
  (365 * year)
  + ((year + 3) / 4)
  - ((year + 99) / 100)
  + ((year + 399) / 400)
  + prior_month_days + day - 1

let ordinal date = ordinal_ymd (Ptime.to_date (Date.midnight_utc date))

let last_date =
  Result.ok_or_failwith
    (Result.map_error (Date.of_string "9999-12-31") ~f:Domain_error.to_string)

let last_ordinal = ordinal last_date

(* Construct the successor from calendar fields, not Date.add_days. *)
let next_ymd date =
  let year, month, day = Ptime.to_date (Date.midnight_utc date) in
  if day < month_length year month then Some (year, month, day + 1)
  else if month < 12 then Some (year, month + 1, 1)
  else if year < 9999 then Some (year + 1, 1, 1)
  else None

(* Ptime's date-time constructor interprets the supplied local offset directly,
   independently of the implementation's UTC-midnight subtraction. *)
let expected_bounds zone date =
  let midnight ymd =
    let utc = Option.value_exn (Ptime.of_date_time (ymd, ((0, 0, 0), 0))) in
    Ptime.of_date_time (ymd, ((0, 0, 0), Time.utc_offset_s_at zone utc))
  in
  Option.bind (next_ymd date) ~f:(fun next ->
      Option.both
        (midnight (Ptime.to_date (Date.midnight_utc date)))
        (midnight next))
