module Domain_error = Error
open Base
module Error = Domain_error

module Account = struct
  type t = string

  let of_string value =
    if String.length value = 0 then
      Error (Error.Invalid_input "Withings account identity required")
    else Ok value

  let to_string value = value
end

type t = {
  id : string;
  user_id : User_id.t;
  withings_user_id : string option;
  token_expires_at : Ptime.t option;
  sync_cursor : int64 option;
  requires_reauthorization : bool;
}

type status =
  | Connected of {
      withings_user_id : string option;
      token_expires_at : Ptime.t option;
      requires_reauthorization : bool;
    }

let status connection =
  Connected
    {
      withings_user_id = connection.withings_user_id;
      token_expires_at = connection.token_expires_at;
      requires_reauthorization = connection.requires_reauthorization;
    }

let connected_account connection =
  match connection.withings_user_id with
  | Some account -> Account.of_string account
  | None -> Error (Error.Invalid_input "Withings account identity required")
