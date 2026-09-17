type source = Manual | Withings

type manual_create = {
  measured_at : Ptime.t option;
  weight_kg : float;
}

type patch = {
  measured_at : Ptime.t option;
  weight_kg : float option;
}

(* Imported observations are constructed only by Withings_sync. *)
type import = {
  external_id : string;
  measured_at : Ptime.t;
  weight_kg : float;
}

let validate_import input =
  if input.external_id <> "" && input.weight_kg > 0.0 then Ok ()
  else Error (Error.Invalid_input "invalid imported weight")

type t = {
  id : Weigh_in_id.t;
  user_id : User_id.t;
  measured_at : Ptime.t;
  weight_kg : float;
  source : source;
  external_id : string option;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  deleted_at : Ptime.t option;
}

let source_to_string = function Manual -> "manual" | Withings -> "withings"

let validate_manual_create _measured_at weight_kg =
  if weight_kg > 0.0 then Ok () else Error (Error.Invalid_input "invalid weight")

let empty_patch = { measured_at = None; weight_kg = None }
