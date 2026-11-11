module Domain_error = Error
open Base
module Error = Domain_error

type serving = { quantity : float; unit : string }

type t = {
  id : Product_id.t;
  user_id : User_id.t;
  version : int;
  name : string;
  basis : string;
  serving : serving option;
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
  serving : (float * string) option;
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

let serving = function
  | None -> Ok None
  | Some (quantity, unit) ->
      if not (Float.is_finite quantity && Float.(quantity > 0.)) then
        Error (Error.Rejected "basis_quantity must be a positive number")
      else if String.is_empty (String.strip unit) then
        Error (Error.Rejected "basis_unit must not be blank")
      else Ok (Some { quantity; unit = String.strip unit })

let validate_input (input : input) =
  let* () = nonblank Product_name input.name in
  let* () = nonblank Product_basis input.basis in
  let* _ = serving input.serving in
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

let of_storage ~id ~user_id ~version ~name ~basis ~serving:raw_serving
    ~calories_kcal ~protein_g ~carbs_g ~fat_g ~source ~created_at ~updated_at =
  let* () =
    validate_input
      {
        name;
        basis;
        serving = raw_serving;
        calories_kcal;
        protein_g;
        carbs_g;
        fat_g;
        source;
      }
  in
  let* serving = serving raw_serving in
  if version < 1 then Error (Error.Storage_decoding "product version")
  else
    Ok
      {
        id;
        user_id;
        version;
        name;
        basis;
        serving;
        calories_kcal;
        protein_g;
        carbs_g;
        fat_g;
        source;
        created_at;
        updated_at;
      }

type portion = {
  calories_kcal : int;
  protein_g : float;
  carbs_g : float option;
  fat_g : float option;
}

let portion (product : t) ~quantity =
  match product.serving with
  | None ->
      Error
        (Error.Rejected
           "this product has no basis_quantity and basis_unit; save it with \
            them before logging by quantity")
  | Some _ when not (Float.is_finite quantity && Float.(quantity > 0.)) ->
      Error (Error.Rejected "quantity must be a positive number")
  | Some serving ->
      let factor = quantity /. serving.quantity in
      let scale grams = grams *. factor in
      let calories = Float.of_int product.calories_kcal *. factor in
      if Float.(calories >= of_int Int.max_value) then
        Error (Error.Numeric_overflow Calories_kcal)
      else
        Ok
          {
            calories_kcal = Float.iround_nearest_exn calories;
            protein_g = scale product.protein_g;
            carbs_g = Option.map product.carbs_g ~f:scale;
            fat_g = Option.map product.fat_g ~f:scale;
          }
