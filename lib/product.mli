type serving = private { quantity : float; unit : string }
(** The [basis] as a number and unit, e.g. 40 and g, so a logged quantity in the
    same unit can be scaled. *)

type t = private {
  id : Product_id.t;
  user_id : User_id.t;
  version : int;  (** Starts at 1; each replacement increments it. *)
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

module Create : sig
  type raw = input
  type t

  val of_raw : raw -> (t, Error.t) result
  val to_raw : t -> raw
end

val of_storage :
  id:Product_id.t ->
  user_id:User_id.t ->
  version:int ->
  name:string ->
  basis:string ->
  serving:(float * string) option ->
  calories_kcal:int ->
  protein_g:float ->
  carbs_g:float option ->
  fat_g:float option ->
  source:string ->
  created_at:Ptime.t ->
  updated_at:Ptime.t ->
  (t, Error.t) result

type portion = {
  calories_kcal : int;
  protein_g : float;
  carbs_g : float option;
  fat_g : float option;
}

val portion : t -> quantity:float -> (portion, Error.t) result
(** Nutrition for [quantity] in the serving's unit, calories rounded to the
    nearest kcal. Rejected when the product has no structured serving. *)
