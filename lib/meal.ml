module Domain_error = Error
open Base
module Error = Domain_error

let ( let* ) result f = Result.bind result ~f

type t = {
  id : Meal_id.t;
  user_id : User_id.t;
  eaten_at : Ptime.t;
  label : string option;
  items : Meal_item.t list;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  deleted_at : Ptime.t option;
}

type patch = { eaten_at : Ptime.t option; label : string option }

let validate_label = function
  | Some label when String.is_empty (String.strip label) ->
      Error (Error.Rejected "label must not be blank")
  | _ -> Ok ()

module Patch = struct
  type raw = patch
  type t = raw

  let of_raw (input : raw) =
    let* () = validate_label input.label in
    Ok input

  let to_raw input = input
end

let of_storage ~id ~user_id ~eaten_at ~label ~items ~created_at ~updated_at
    ~deleted_at =
  let* () = validate_label label in
  if List.is_empty items then
    Error (Error.Storage_decoding "meal without items")
  else
    Ok
      {
        id;
        user_id;
        eaten_at;
        label;
        items;
        created_at;
        updated_at;
        deleted_at;
      }

type totals = {
  meal_count : int;
  calories_kcal : int;
  protein_g : float;
  carbs_g : float option;
  fat_g : float option;
}

let finite value =
  if Float.is_finite value then Ok value
  else Error (Error.Unrepresentable_result Daily_meals)

let add_calories left right =
  if left > Int.max_value - right then
    Error (Error.Numeric_overflow Calories_kcal)
  else Ok (left + right)

let add_optional left right =
  match (left, right) with
  | None, other | other, None -> Ok other
  | Some a, Some b -> Result.map (finite (a +. b)) ~f:Option.some

let add_totals left right =
  let* calories_kcal = add_calories left.calories_kcal right.calories_kcal in
  let* protein_g = finite (left.protein_g +. right.protein_g) in
  let* carbs_g = add_optional left.carbs_g right.carbs_g in
  let* fat_g = add_optional left.fat_g right.fat_g in
  Ok
    {
      meal_count = left.meal_count + right.meal_count;
      calories_kcal;
      protein_g;
      carbs_g;
      fat_g;
    }

let item_totals meal_count (item : Meal_item.t) =
  {
    meal_count;
    calories_kcal = item.calories_kcal;
    protein_g = item.protein_g;
    carbs_g = item.carbs_g;
    fat_g = item.fat_g;
  }

let totals meal =
  match meal.items with
  | [] -> Error (Error.Storage_decoding "meal without items")
  | first :: rest ->
      List.fold_result rest ~init:(item_totals 1 first) ~f:(fun sum item ->
          add_totals sum (item_totals 0 item))
