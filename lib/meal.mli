type product_link = {
  product_id : Product_id.t;
  product_version : int;
  quantity : float;  (** In the product's serving unit. *)
}
(** Where a meal's nutrition came from when it was logged from a product. The
    meal still stores its own values; the link only records their origin. *)

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
  product : product_link option;
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
  product:product_link option ->
  created_at:Ptime.t ->
  updated_at:Ptime.t ->
  deleted_at:Ptime.t option ->
  (t, Error.t) result

val empty_patch : patch

type snapshot = {
  eaten_at : Ptime.t;
  description : string;
  calories_kcal : int;
  protein_g : float;
  carbs_g : float option;
  fat_g : float option;
  confidence : float option;
  estimate_source : string option;
  notes : string option;
  product : product_link option;
}

type revision = {
  revision : int;  (** 1 for the first change. *)
  revised_at : Ptime.t;
  reason : string option;
  previous : snapshot;  (** The meal as it was before this change. *)
}

val changes_nutrition : Patch.t -> bool
(** Whether the patch sets calories or a macro, which detaches a product link
    because the values no longer follow from the product. *)

type from_product = {
  product_id : Product_id.t;
  quantity : float;  (** In the product's serving unit. *)
  description : string option;  (** Defaults to the product name. *)
  confidence : float option;
  estimate_source : string option;  (** Defaults to the product's source. *)
  notes : string option;
  eaten_at : Ptime.t option;
}
