type product_link = {
  product_id : Product_id.t;
  product_version : int;
  quantity : float;  (** In [unit]. *)
  unit : string;
      (** The product's basis unit when the item was logged, so a later version
          in another unit is never applied to this quantity. *)
}
(** Where an item's nutrition came from when it was logged from a product. The
    item still stores its own values; the link only records their origin. *)

type t = private {
  id : Meal_item_id.t;
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
}
(** One thing eaten within a meal, with its nutrition estimate and the
    estimate's provenance. *)

type patch = {
  description : string option;
  calories_kcal : int option;
  protein_g : float option;
  carbs_g : float option;
  fat_g : float option;
  confidence : float option;
  estimate_source : string option;
  notes : string option;
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
  id:Meal_item_id.t ->
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
  (t, Error.t) result

val empty_patch : patch

type snapshot = {
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
  previous : snapshot;  (** The item as it was before this change. *)
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
}

(** How an item is given: estimated nutrition, or a product and quantity for the
    server to scale. *)
type entry = Estimated of Create.t | From_product of from_product

val rescale :
  t -> quantity:float -> (Product.portion * product_link, Error.t) result
(** The item's own values scaled to a new quantity of the same product version,
    calories rounded to the nearest kcal. Rejected for an item not logged from a
    product. *)
