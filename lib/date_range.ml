module Domain_error = Error
open Base
module Error = Domain_error

let ( let* ) result f = Result.bind result ~f

type t = { first : Date.t; last : Date.t }

let max_day_count = 366

let make ~first ~last =
  let day_count = Date.days_between ~first ~last + 1 in
  if day_count < 1 then
    Error
      (Error.Reversed_date_range
         { from_date = Date.to_string first; to_date = Date.to_string last })
  else if day_count > max_day_count then
    Error (Error.Date_range_too_long { day_count; max_day_count })
  else Ok { first; last }

let first range = range.first
let last range = range.last
let day_count range = Date.days_between ~first:range.first ~last:range.last + 1

let dates range =
  List.init (day_count range) ~f:(fun index ->
      match Date.add_days range.first index with
      | Ok date -> date
      (* make proves every listed date lies between representable endpoints. *)
      | Error _ -> assert false)

let mem range date =
  Date.compare range.first date <= 0 && Date.compare date range.last <= 0

let utc_bounds zone range =
  let* start, _ = Time.local_day_bounds zone range.first in
  let* _, stop = Time.local_day_bounds zone range.last in
  Ok (start, stop)
