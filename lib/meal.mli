(** A meal is an occasion: when something was eaten and, optionally, what to
    call it. Its nutrition is the sum of its items, never stored. *)

type t = private {
  id : Meal_id.t;
  user_id : User_id.t;
  eaten_at : Ptime.t;
  label : string option;  (** e.g. lunch. *)
  items : Meal_item.t list;  (** Never empty, in the order they were added. *)
  created_at : Ptime.t;
  updated_at : Ptime.t;
  deleted_at : Ptime.t option;
}

type patch = { eaten_at : Ptime.t option; label : string option }

module Patch : sig
  type raw = patch
  type t

  val of_raw : raw -> (t, Error.t) result
  val to_raw : t -> raw
end

val validate_label : string option -> (unit, Error.t) result
(** A label, when given, is not blank. *)

val of_storage :
  id:Meal_id.t ->
  user_id:User_id.t ->
  eaten_at:Ptime.t ->
  label:string option ->
  items:Meal_item.t list ->
  created_at:Ptime.t ->
  updated_at:Ptime.t ->
  deleted_at:Ptime.t option ->
  (t, Error.t) result
(** Fails without items. *)

type totals = {
  meal_count : int;
  calories_kcal : int;
  protein_g : float;
  carbs_g : float option;  (** [None] when no item recorded it. *)
  fat_g : float option;
}

val totals : t -> (totals, Error.t) result
(** One meal's totals; fails rather than overflowing. *)

val add_totals : totals -> totals -> (totals, Error.t) result
