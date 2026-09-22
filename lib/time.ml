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
