module Domain_error = Error
open Base
module Error = Domain_error

(* Represented as the date's UTC midnight, so day arithmetic is Ptime arithmetic. *)
type t = Ptime.t

let of_ymd (year, month, day) =
  Ptime.of_date_time ((year, month, day), ((0, 0, 0), 0))

let of_string value =
  let invalid = Error (Error.Invalid_date value) in
  match (String.length value, String.split value ~on:'-') with
  | 10, [ year; month; day ]
    when String.length year = 4
         && String.length month = 2
         && String.length day = 2
         && String.for_all
              (year ^ month ^ day)
              ~f:(fun c -> Char.(c >= '0' && c <= '9')) -> (
      match
        (Int.of_string_opt year, Int.of_string_opt month, Int.of_string_opt day)
      with
      | Some year, Some month, Some day -> (
          match of_ymd (year, month, day) with
          | Some date -> Ok date
          | None -> invalid)
      | _ -> invalid)
  | _ -> invalid

let to_string date =
  let year, month, day = Ptime.to_date date in
  Printf.sprintf "%04d-%02d-%02d" year month day

let equal = Ptime.equal
let compare = Ptime.compare

let of_instant ~utc_offset_s instant =
  let date, (_, actual_offset_s) =
    Ptime.to_date_time ~tz_offset_s:utc_offset_s instant
  in
  if actual_offset_s <> utc_offset_s then
    Error
      (Error.Calendar_local_date
         { instant = Ptime.to_rfc3339 instant; utc_offset_s })
  else
    match of_ymd date with
    | Some date -> Ok date
    (* Ptime returned a date in its own calendar with the requested offset. *)
    | None -> assert false

let add_days date days =
  match
    Option.bind (Ptime.Span.of_d_ps (days, 0L)) ~f:(Ptime.add_span date)
  with
  | Some date -> Ok date
  | None -> Error (Error.Calendar_date_addition { date = to_string date; days })

(* Both are midnights, so the span is a whole number of days. *)
let days_between ~first ~last = fst (Ptime.Span.to_d_ps (Ptime.diff last first))
let midnight_utc date = date
