module Domain_error = Error
open Base
module Error = Domain_error

type t = {
  id : Meal_id.t;
  user_id : User_id.t;
  eaten_at : Ptime.t;
  description : string;
  calories_kcal : int;
  protein_g : float;
  carbs_g : float option;
  fat_g : float option;
  confidence : float option;
  estimate_source : string option;
  notes : string option;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  deleted_at : Ptime.t option;
}

type patch = {
  description : string option;
  calories_kcal : int option;
  protein_g : float option;
  carbs_g : float option;
  fat_g : float option;
  confidence : float option;
  estimate_source : string option;
  notes : string option;
  eaten_at : Ptime.t option;
}

type create = {
  description : string;
  calories_kcal : int;
  protein_g : float;
  carbs_g : float option;
  fat_g : float option;
  confidence : float option;
  estimate_source : string option;
  notes : string option;
  eaten_at : Ptime.t option;
}

type totals = {
  meal_count : int;
  calories_kcal : int;
  protein_g : float;
  carbs_g : float option;
  fat_g : float option;
}

let ( let* ) result f = Result.bind result ~f

let nonblank field value =
  if String.is_empty (String.strip value) then
    Error (Error.Invalid_field { field; reason = Blank })
  else Ok ()

let grams field value =
  Result.map_error (Nutrition.Grams.of_float value) ~f:(function
    | Error.Invalid_field { reason; _ } ->
        Error.Invalid_field { field = Numeric field; reason }
    | error -> error)

let optional f = function
  | None -> Ok ()
  | Some value -> Result.map (f value) ~f:(fun _ -> ())

let validate_create (input : create) =
  let* () = nonblank Description input.description in
  let* _ = Nutrition.Calories.of_int input.calories_kcal in
  let* _ = grams Protein_g input.protein_g in
  let* () = optional (grams Carbs_g) input.carbs_g in
  let* () = optional (grams Fat_g) input.fat_g in
  optional Nutrition.Confidence.of_float input.confidence

let validate_patch (input : patch) =
  let* () = optional (nonblank Description) input.description in
  let* () = optional Nutrition.Calories.of_int input.calories_kcal in
  let* () = optional (grams Protein_g) input.protein_g in
  let* () = optional (grams Carbs_g) input.carbs_g in
  let* () = optional (grams Fat_g) input.fat_g in
  optional Nutrition.Confidence.of_float input.confidence

let empty_patch : patch =
  {
    description = None;
    calories_kcal = None;
    protein_g = None;
    carbs_g = None;
    fat_g = None;
    confidence = None;
    estimate_source = None;
    notes = None;
    eaten_at = None;
  }

module Create = struct
  type raw = create
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

let of_storage ~id ~user_id ~eaten_at ~description ~calories_kcal ~protein_g
    ~carbs_g ~fat_g ~confidence ~estimate_source ~notes ~created_at ~updated_at
    ~deleted_at =
  let* () =
    validate_create
      {
        description;
        calories_kcal;
        protein_g;
        carbs_g;
        fat_g;
        confidence;
        estimate_source;
        notes;
        eaten_at = Some eaten_at;
      }
  in
  Ok
    {
      id;
      user_id;
      eaten_at;
      description;
      calories_kcal;
      protein_g;
      carbs_g;
      fat_g;
      confidence;
      estimate_source;
      notes;
      created_at;
      updated_at;
      deleted_at;
    }
