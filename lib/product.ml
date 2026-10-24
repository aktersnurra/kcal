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

let validate_input input =
  let valid_macro value = Float.is_finite value && value >= 0.0 in
  let valid_optional = function None -> true | Some value -> valid_macro value in
  if String.trim input.name = "" || String.trim input.basis = ""
     || String.trim input.source = "" || input.calories_kcal < 0
     || not (valid_macro input.protein_g)
     || not (valid_optional input.carbs_g) || not (valid_optional input.fat_g)
  then Error (Error.Invalid_input "invalid product")
  else Ok ()
