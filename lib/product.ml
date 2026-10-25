module Domain_error = Error
open Base
module Error = Domain_error

type t = {
  id : Product_id.t;
  user_id : User_id.t;
  name : string;
  basis : string;
  calories_kcal : int;
  protein_g : float;
  carbs_g : float option;
  fat_g : float option;
  source : string;
  created_at : Ptime.t;
  updated_at : Ptime.t;
}

type input = {
  name : string;
  basis : string;
  calories_kcal : int;
  protein_g : float;
  carbs_g : float option;
  fat_g : float option;
  source : string;
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

let validate_input (input : input) =
  let* () = nonblank Product_name input.name in
  let* () = nonblank Product_basis input.basis in
  let* () = nonblank Product_source input.source in
  let* _ = Nutrition.Calories.of_int input.calories_kcal in
  let* _ = grams Protein_g input.protein_g in
  let* () = optional (grams Carbs_g) input.carbs_g in
  optional (grams Fat_g) input.fat_g

module Create = struct
  type raw = input
  type t = raw

  let of_raw input =
    let* () = validate_input input in
    Ok input

  let to_raw input = input
end

let of_storage ~id ~user_id ~name ~basis ~calories_kcal ~protein_g ~carbs_g
    ~fat_g ~source ~created_at ~updated_at =
  let* () =
    validate_input
      { name; basis; calories_kcal; protein_g; carbs_g; fat_g; source }
  in
  Ok
    {
      id;
      user_id;
      name;
      basis;
      calories_kcal;
      protein_g;
      carbs_g;
      fat_g;
      source;
      created_at;
      updated_at;
    }
