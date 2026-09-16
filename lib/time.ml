let invalid_timestamp = Error.Invalid_input "invalid timestamp"

let parse_offset_datetime value =
  match Ptime.of_rfc3339 ~strict:true value with
  | Ok (time, Some _, _) -> Ok time
  | Ok (_, None, _) -> Error invalid_timestamp
  | Error _ -> Error invalid_timestamp

let to_utc_string time = Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 time
