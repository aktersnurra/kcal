module Domain_error = Error
open Base
module Error = Domain_error

module Account = struct
  type t = string

  let of_string value =
    if String.is_empty value then
      Error (Error.Storage_decoding "Withings account")
    else Ok value

  let to_string value = value
end

type connected = {
  id : string;
  user_id : User_id.t;
  account : Account.t;
  token_expires_at : Ptime.t;
  sync_cursor : int64 option;
}

type status =
  | Disconnected
  | Connected of connected
  | Reauthorization_required of connected

let decode ~id ~user_id ~withings_user_id ~token_expires_at ~sync_cursor =
  match Account.of_string withings_user_id with
  | Error _ as error -> error
  | Ok account -> Ok { id; user_id; account; token_expires_at; sync_cursor }
