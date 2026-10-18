type t =
  | Unauthorized
  | Not_found
  | Invalid_input of string
  | Conflict of string
  | Storage_error of string
  | Invalid_date of string
  | Unsupported_timezone of string
  | Reversed_date_range of { from_date : string; to_date : string }
  | Date_range_too_long of { day_count : int; max_day_count : int }
  | Nonpositive_argument of string

let to_string = function
  | Unauthorized -> "unauthorized"
  | Not_found -> "not found"
  | Invalid_input reason -> "invalid input: " ^ reason
  | Conflict reason -> "conflict: " ^ reason
  | Storage_error reason -> "storage error: " ^ reason
  | Invalid_date value -> Printf.sprintf "invalid date %S: expected YYYY-MM-DD" value
  | Unsupported_timezone value -> Printf.sprintf "unsupported timezone %S" value
  | Reversed_date_range { from_date; to_date } ->
      Printf.sprintf "from_date %s is after to_date %s (to_date defaults to today in the requested timezone)" from_date to_date
  | Date_range_too_long { day_count; max_day_count } ->
      Printf.sprintf "date range covers %d days; at most %d are allowed, so split it into several calls" day_count max_day_count
  | Nonpositive_argument name -> Printf.sprintf "%s must be positive" name
