type t =
  | Unauthorized
  | Not_found
  | Invalid_input of string
  | Conflict of string
  | Storage_error of string

let to_string = function
  | Unauthorized -> "unauthorized"
  | Not_found -> "not found"
  | Invalid_input reason -> "invalid input: " ^ reason
  | Conflict reason -> "conflict: " ^ reason
  | Storage_error reason -> "storage error: " ^ reason
