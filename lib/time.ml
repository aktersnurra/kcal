let invalid_timestamp = Error.Invalid_input "invalid timestamp"

let parse_offset_datetime value =
  match Ptime.of_rfc3339 ~strict:true value with
  | Ok (time, Some _, _) -> Ok time
  | Ok (_, None, _) -> Error invalid_timestamp
  | Error _ -> Error invalid_timestamp

let to_utc_string time = Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 time

let to_date_string time =
  let (year, month, day), _ = Ptime.to_date_time ~tz_offset_s:0 time in
  Printf.sprintf "%04d-%02d-%02d" year month day

(* A bare calendar date (no time-of-day or offset), interpreted as UTC midnight. *)
let parse_date value =
  match String.length value, String.split_on_char '-' value with
  | 10, [ year; month; day ]
    when String.length year = 4 && String.length month = 2 && String.length day = 2 -> (
      try
        match Ptime.of_date_time ((int_of_string year, int_of_string month, int_of_string day), ((0, 0, 0), 0)) with
        | Some time -> Ok time
        | None -> Error invalid_timestamp
      with Failure _ -> Error invalid_timestamp)
  | _ -> Error invalid_timestamp

(* [start, stop) UTC calendar-day bounds covering [time]. *)
let day_bounds time =
  let (year, month, day), _ = Ptime.to_date_time ~tz_offset_s:0 time in
  let start = Option.get (Ptime.of_date_time ((year, month, day), ((0, 0, 0), 0))) in
  let stop = Option.get (Ptime.add_span start (Ptime.Span.of_int_s 86400)) in
  (start, stop)

(* Time zones for bucketing instants into calendar days.  No tz database is
   linked in, so a zone is either a fixed UTC offset or a fixed standard offset
   with the EU summer-time rules (last Sunday of March to last Sunday of
   October, switching at 01:00 UTC). *)
type zone = Fixed of int | Eu of int

let utc = Fixed 0

let named_zones =
  [ ("UTC", Fixed 0);
    ("Europe/London", Eu 0); ("Europe/Dublin", Eu 0); ("Europe/Lisbon", Eu 0);
    ("Europe/Stockholm", Eu 3600); ("Europe/Oslo", Eu 3600); ("Europe/Copenhagen", Eu 3600);
    ("Europe/Berlin", Eu 3600); ("Europe/Amsterdam", Eu 3600); ("Europe/Brussels", Eu 3600);
    ("Europe/Paris", Eu 3600); ("Europe/Madrid", Eu 3600); ("Europe/Rome", Eu 3600);
    ("Europe/Vienna", Eu 3600); ("Europe/Zurich", Eu 3600); ("Europe/Prague", Eu 3600);
    ("Europe/Warsaw", Eu 3600);
    ("Europe/Helsinki", Eu 7200); ("Europe/Athens", Eu 7200); ("Europe/Kyiv", Eu 7200) ]

let invalid_zone = Error.Invalid_input "invalid timezone"

(* Accepts a supported IANA name or a fixed offset such as "Z", "+02:00". *)
let parse_zone value =
  match List.assoc_opt value named_zones with
  | Some zone -> Ok zone
  | None -> (
      match Ptime.of_rfc3339 ~strict:true ("2000-01-01T00:00:00" ^ value) with
      | Ok (_, Some offset, _) when abs offset <= 14 * 3600 -> Ok (Fixed offset)
      | _ -> Error invalid_zone)

let shift time seconds = Option.get (Ptime.add_span time (Ptime.Span.of_int_s seconds))

let eu_transition year month =
  let at day = Option.get (Ptime.of_date_time ((year, month, day), ((1, 0, 0), 0))) in
  let days_back =
    match Ptime.weekday (at 31) with
    | `Sun -> 0 | `Mon -> 1 | `Tue -> 2 | `Wed -> 3 | `Thu -> 4 | `Fri -> 5 | `Sat -> 6
  in
  at (31 - days_back)

(* Seconds east of UTC in effect at the instant [time]. *)
let offset_at zone time =
  match zone with
  | Fixed offset -> offset
  | Eu standard ->
      let (year, _, _), _ = Ptime.to_date_time time in
      if Ptime.compare time (eu_transition year 3) >= 0 && Ptime.compare time (eu_transition year 10) < 0
      then standard + 3600
      else standard

let date_string_of_triple (year, month, day) = Printf.sprintf "%04d-%02d-%02d" year month day

(* The calendar date, as YYYY-MM-DD, that [time] falls on in [zone]. *)
let local_date_string zone time =
  let date, _ = Ptime.to_date_time ~tz_offset_s:(offset_at zone time) time in
  date_string_of_triple date

(* The UTC instant at which the local calendar day starting at [date] (a bare
   date, as returned by [parse_date]) begins in [zone]. *)
let local_midnight zone date =
  let guess = shift date (-offset_at zone date) in
  shift date (-offset_at zone guess)

(* [start, stop) UTC bounds of the local calendar day [date] in [zone]. *)
let local_day_bounds zone date = (local_midnight zone date, local_midnight zone (shift date 86400))

let max_range_days = 366

(* Every calendar date from [from_date] to [to_date] inclusive, as YYYY-MM-DD. *)
let date_range ~from_date ~to_date =
  match parse_date from_date, parse_date to_date with
  | Ok first, Ok last ->
      let days = int_of_float (Ptime.Span.to_float_s (Ptime.diff last first) /. 86400.) + 1 in
      if days < 1 || days > max_range_days then Error (Error.Invalid_input "invalid date range")
      else
        Ok (List.init days (fun index ->
          date_string_of_triple (Ptime.to_date (shift first (index * 86400)))))
  | Error e, _ | _, Error e -> Error e
