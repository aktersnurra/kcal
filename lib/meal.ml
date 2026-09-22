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

let invalid_input = Error.Invalid_input "invalid meal"

let is_nonnegative value = value >= 0.0

let validate_create input =
  let valid_optional_macro = function None -> true | Some value -> is_nonnegative value in
  let valid_confidence = function
    | None -> true
    | Some value -> value >= 0.0 && value <= 1.0
  in
  if
    String.trim input.description = ""
    || input.calories_kcal < 0
    || not (is_nonnegative input.protein_g)
    || not (valid_optional_macro input.carbs_g)
    || not (valid_optional_macro input.fat_g)
    || not (valid_confidence input.confidence)
  then Error invalid_input
  else Ok ()

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
