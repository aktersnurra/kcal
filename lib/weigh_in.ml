module Domain_error = Error
open Base
module Error = Domain_error

module Imported_identity = struct
  type t = { account_id : string; group_id : string }

  let of_external_id value =
    match String.lsplit2 value ~on:':' with
    | Some (account_id, group_id)
      when (not (String.is_empty account_id)) && not (String.is_empty group_id)
      ->
        Ok { account_id; group_id }
    | _ -> Error (Error.Invalid_field { field = External_id; reason = Blank })

  let external_id identity = identity.account_id ^ ":" ^ identity.group_id
end

type source = Manual | Withings of Imported_identity.t
type manual_create = { measured_at : Ptime.t option; weight_kg : float }
type patch = { measured_at : Ptime.t option; weight_kg : float option }
type import = { external_id : string; measured_at : Ptime.t; weight_kg : float }

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

let ( let* ) result f = Result.bind result ~f

let optional f = function
  | None -> Ok ()
  | Some value -> Result.map (f value) ~f:(fun _ -> ())

let source_to_string = function Manual -> "manual" | Withings _ -> "withings"

let validate_weight weight_kg =
  Result.map (Nutrition.Kilograms.of_float weight_kg) ~f:(fun _ -> ())

let validate_create (input : manual_create) = validate_weight input.weight_kg

let validate_patch (input : patch) =
  optional Nutrition.Kilograms.of_float input.weight_kg

let validate_import (input : import) =
  let* _identity = Imported_identity.of_external_id input.external_id in
  validate_weight input.weight_kg

let empty_patch = { measured_at = None; weight_kg = None }

module Create = struct
  type raw = manual_create
  type t = raw

  let of_raw input =
    let* () = validate_create input in
    Ok input

  let to_raw input = input
end

module Patch = struct
  type raw = patch
  type t = raw

  let of_raw input =
    let* () = validate_patch input in
    Ok input

  let to_raw input = input
end

module Import = struct
  type raw = import
  type t = raw

  let of_raw input =
    let* () = validate_import input in
    Ok input

  let to_raw input = input
end

let of_storage ~id ~user_id ~measured_at ~weight_kg ~source ~created_at
    ~updated_at ~deleted_at =
  let* () = validate_weight weight_kg in
  Ok
    {
      id;
      user_id;
      measured_at;
      weight_kg;
      source;
      external_id =
        (match source with
        | Manual -> None
        | Withings identity -> Some (Imported_identity.external_id identity));
      created_at;
      updated_at;
      deleted_at;
    }
