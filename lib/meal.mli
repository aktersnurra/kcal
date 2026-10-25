type t = private {
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

module Create : sig
  type raw = create
  type t

  val of_raw : raw -> (t, Error.t) result
  val to_raw : t -> raw
end

module Patch : sig
  type raw = patch
  type t

  val of_raw : raw -> (t, Error.t) result
  val to_raw : t -> raw
end

val of_storage :
  id:Meal_id.t ->
  user_id:User_id.t ->
  eaten_at:Ptime.t ->
  description:string ->
  calories_kcal:int ->
  protein_g:float ->
  carbs_g:float option ->
  fat_g:float option ->
  confidence:float option ->
  estimate_source:string option ->
  notes:string option ->
  created_at:Ptime.t ->
  updated_at:Ptime.t ->
  deleted_at:Ptime.t option ->
  (t, Error.t) result

val empty_patch : patch
